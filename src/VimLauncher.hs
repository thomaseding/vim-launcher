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
    True  -> helpExit
    False -> pure ()

  case oBadOptions opts of
    [] -> pure ()
    badOpts -> badOptionsExit badOpts

  case args of

    [str] -> do
      _ <- tryExit $ openExactPath str

      allFiles <- ls

      _ <- tryExit $ openPartialPath allFiles str

      _ <- tryExit $ case fromHaskellPath str of
        Nothing -> pure $ Left []
        Just str' -> openPartialPath allFiles str'

      _ <- tryExit $ openDef opts str
        [ Class
        , Constructor
        , Data
        , Newtype
        , Type
        , Variable Global
        ]
      pure ()

    (viewDef -> Just def) : [str] -> do
      _ <- tryExit $ openDef opts str [def]
      pure ()

    _ -> badArgsExit

  putStrLn "Could not find a command."
  Exit.exitFailure

helpExit :: IO ()
helpExit = do
  putStrLn "Usage: TODO"
  Exit.exitFailure

data Options = Options
  { oBadOptions :: [String]
  , oHelp       :: Bool
  , oIgnoreCase :: Bool
  , oGotoLine   :: Bool
  }

newtype Keyword = Keyword { unKeyword :: [String] }

getArgs :: IO (Options, [String])
getArgs = do
  args <- Env.getArgs
  let (opts, args') = List.partition isOption args
      exists = any (`elem` opts) . unKeyword

      help       = Keyword ["-h", "--help"]
      ignoreCase = Keyword ["-i"]
      gotoLine   = Keyword ["+"]

      badOpts = let
        kws = unKeyword =<<
          [ help
          , ignoreCase
          , gotoLine
          ]
        in filter (`notElem` kws) opts

      opts' = Options
        { oBadOptions = badOpts
        , oHelp       = exists help
        , oIgnoreCase = exists ignoreCase
        , oGotoLine   = exists gotoLine
        }

  pure (opts', args')

isOption :: String -> Bool
isOption = \case
  '-' : _ -> True
  '+' : _ -> True
  _ -> False

tryExit :: IO OpenResult -> IO [FilePath]
tryExit action = action >>= \case
  Left files -> pure files
  Right args -> do
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

badOptionsExit :: [String] -> IO a
badOptionsExit badOpts = do
  putStrLn $ "Bad options: " ++ show badOpts
  Exit.exitFailure

badArgsExit :: IO a
badArgsExit = do
  putStrLn "Bad args"
  Exit.exitFailure

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
    globalVar name = pure $ "^" ++ name ++ "\\s*::"
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
      Variable scope -> do
        guard $ isVariable identifier
        case scope of
          Global -> globalVar identifier
          Local -> globalVar $ "\\s\\+" ++ identifier

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

type OpenResult = Either [FilePath] VimArgs

openDef :: Options -> String -> [DefType] -> IO OpenResult
openDef opts name defs = case toGrepQuery name defs of
  Nothing -> pure $ Left []
  Just query -> do
    files <- do
      let ic = case oIgnoreCase opts of
            True  -> ["-i"]
            False -> []
      grep $ ic ++ [query]
    case files of
      [result] -> do
        let (file, lineNumber) = parseGrepFilePath result
            gotoLine = case oGotoLine opts of
              True  -> ['+' : show lineNumber]
              False -> []
        pure $ Right $ VimArgs $ [file] ++ gotoLine
      _ -> pure $ Left files

parseGrepFilePath :: String -> (FilePath, Int)
parseGrepFilePath s = let
  junk = ("", -1)
  in case span (/= ':') s of
    (file, ':' : s') -> case span (/= ':') s' of
      (lineNumber, ':' : _) -> (file, read lineNumber)
      _ -> junk
    _ -> junk

openExactPath :: String -> IO OpenResult
openExactPath str = Dir.doesPathExist str >>= \case
  False -> pure $ Left []
  True  -> pure $ Right $ VimArgs [str]

openPartialPath :: [FilePath] -> String -> IO OpenResult
openPartialPath allFiles = pure . openPartialPath' allFiles

openPartialPath' :: [FilePath] -> String -> OpenResult
openPartialPath' allFiles str = case findFiles allFiles str of
  [file] -> Right $ VimArgs [file]
  files -> Left files

findFiles :: [FilePath] -> String -> [String]
findFiles files s = filter (s `List.isSuffixOf`) files

