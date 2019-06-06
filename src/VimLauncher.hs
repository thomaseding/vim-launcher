{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module VimLauncher (
  main,
) where

import qualified Control.Exception as Exn
import           Control.Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Prelude hiding (pred)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Process as Proc

main :: IO ()
main = getArgs >>= \(opts, args) -> do

  case oHelp opts of
    True  -> doHelp
    False -> pure ()

  case args of

    [str] -> do
      try $ openExact str

      files <- ls

      try $ openPartialPath files str

      try $ case fromHaskellPath str of
        Nothing -> pure Nothing
        Just str' -> openPartialPath files str'

      try $ openDef opts str
        [ Class
        , Constructor
        , Data
        , Newtype
        , Type
        , Variable Global
        ]

      noVimArgs

    (viewDef -> Just def) : [str] -> do
      try $ openDef opts str [def]
      noVimArgs

    _ -> do
      badArgs

doHelp :: IO ()
doHelp = do
  putStrLn "Usage: TODO"
  Exit.exitFailure

data Options = Options
  { oHelp :: Bool
  , oIgnoreCase :: Bool
  }

getArgs :: IO (Options, [String])
getArgs = do
  args <- Env.getArgs
  let (opts, args') = List.partition isOption args
      exists = any (`elem` opts)
      opts' = Options
        { oHelp       = exists ["-h", "--help"]
        , oIgnoreCase = exists ["-i"]
        }
  pure (opts', args')

isOption :: String -> Bool
isOption = \case
  '-' : _ -> True
  _ -> False

try :: IO (Maybe VimArgs) -> IO ()
try action = action >>= \case
  Nothing -> pure ()
  Just args -> do
    print args
    Exit.exitSuccess

fromHaskellPath :: String -> Maybe String
fromHaskellPath s = do
  s' <- sequence $ flip map s $ \case
    '/' -> Nothing
    '.' -> Just '/'
    c -> Just c
  pure $ s' ++ ".hs"

newtype VimArgs = VimArgs [String]

instance Show VimArgs where
  show (VimArgs strs) = unwords strs -- TODO: Escape string characters.

rawGitProc :: String -> [String] -> IO String
rawGitProc name args = Proc.readProcess "git" (name : args) []

gitProc :: String -> [String] -> IO [String]
gitProc name args = let
  action = fmap lines $ rawGitProc name args
  in Exn.catch action $ \e -> let
    _ = e :: Exn.SomeException
    in pure []

grep :: [String] -> IO [String]
grep args = gitProc "grep" $ "-n" : args

ls :: IO [FilePath]
ls = gitProc "ls-files" ["--cached", "--others"]

badArgs :: IO a
badArgs = do
  putStrLn "Bad args"
  Exit.exitFailure

noVimArgs :: IO ()
noVimArgs = do
  putStrLn "Could not find a command."
  Exit.exitFailure

fromSingle :: [a] -> Maybe a
fromSingle = \case
  [x] -> Just x
  _ -> Nothing

data Scope = Local | Global

data DefType
  = Class
  | Constructor
  | Data
  -- | Instance
  | Newtype
  | Type
  | Variable Scope

viewDef :: String -> Maybe DefType
viewDef = \case
  "class" -> Just Class
  "cons" -> Just Constructor
  "constructor" -> Just Constructor
  "data" -> Just Data
  "local" -> Just $ Variable Local
  "new" -> Just Newtype
  "newtype" -> Just Newtype
  "type" -> Just Type
  "var" -> Just $ Variable Global
  "variable" -> Just $ Variable Global
  _ -> Nothing

class ToGrepQuery a where
  toGrepQuery :: String -> a -> Maybe String

instance ToGrepQuery DefType where
  toGrepQuery identifier = let
    kwDecl kwName = "^\\s*" ++ kwName ++ "\\s\\+" ++ identifier ++ "\\>"
    in \case
      Class -> do
        guard $ isType identifier
        pure $ kwDecl "class"
      Constructor -> do
        guard $ isType identifier
        pure $ "\\s[=|]\\s*" ++ identifier ++ "\\>"
      Data -> do
        guard $ isType identifier
        pure $ kwDecl "data"
      Newtype -> do
        guard $ isType identifier
        pure $ kwDecl "newtype"
      Type -> do
        guard $ isType identifier
        pure $ kwDecl "type"
      Variable scope -> case scope of
        Global -> do
          guard $ isVariable identifier
          pure $ "^" ++ identifier ++ "\\s*::"
        Local -> do
          guard $ isVariable identifier
          pure $ "^\\s\\+" ++ identifier ++ "\\s*::"

instance ToGrepQuery [DefType] where
  toGrepQuery identifier defs = let
    toQuery = toGrepQuery identifier
    queries = Maybe.mapMaybe toQuery defs
    in case queries of
      [] -> Nothing
      _ -> Just $ "\\(" ++ List.intercalate "\\)\\|\\(" queries ++ "\\)"

isType :: String -> Bool
isType = \case
  c : _ -> List.all id
    [ Char.isAlpha c
    , Char.toUpper c == c
    ]
  _ -> False

isVariable :: String -> Bool
isVariable = \case
  c : _ -> List.all id
    [ Char.isAlpha c || c == '_'
    , Char.toLower c == c
    ]
  _ -> False

openDef :: Options -> String -> [DefType] -> IO (Maybe VimArgs)
openDef opts name defs = case toGrepQuery name defs of
  Nothing -> pure Nothing
  Just query -> do
    mResult <- fmap fromSingle $ do
      let ic = case oIgnoreCase opts of
            True  -> ["-i"]
            False -> []
      grep $ ic ++ [query]
    case mResult of
      Nothing -> pure Nothing
      Just result -> do
        let (file, lineNumber) = parseGrepFilePath result
        pure $ Just $ VimArgs [file, "+" ++ show lineNumber]

parseGrepFilePath :: String -> (FilePath, Int)
parseGrepFilePath s = case span (/= ':') s of
  (file, ':' : s') -> case span (/= ':') s' of
    (lineNumber, ':' : _) -> (file, read lineNumber)
    _ -> junk
  _ -> junk

  where
    junk = ("", -1)

openExact :: String -> IO (Maybe VimArgs)
openExact str = Dir.doesPathExist str >>= \case
  False -> pure Nothing
  True -> pure $ Just $ VimArgs [str]

openPartialPath :: [FilePath] -> String -> IO (Maybe VimArgs)
openPartialPath files str = findFile files str >>= \case
  Left _ -> pure Nothing
  Right file -> pure $ Just $ VimArgs [file]

findFile :: [FilePath] -> String -> IO (Either String FilePath)
findFile files s = let
  files' = filter (s `List.isSuffixOf`) files
  in pure $ case files' of
    [file] -> Right file
    [] -> Left "No such file"
    _ -> Left "Too many files"

