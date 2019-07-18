{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module VimLauncher (
  main,
) where

import qualified Control.Exception as Exn
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude hiding (pred)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Process as Proc

data ProgState = ProgState
  { stMatchingFiles :: Set FilePath
  }

type Prog = StateT ProgState IO

runProg :: Prog () -> IO (Set FilePath)
runProg action = do
  let st = ProgState
        { stMatchingFiles = mempty
        }
  st' <- State.execStateT action st
  pure $ stMatchingFiles st'

main :: IO ()
main = getArgs >>= \(opts, args) -> do

  case oHelp opts of
    True  -> helpExit
    False -> pure ()

  case oBadOptions opts of
    [] -> pure ()
    badOpts -> badOptionsExit badOpts

  matchingFiles <- runProg $ case args of

    [str] -> do
      tryExit $ openExactPath str

      allFiles <- liftIO ls

      tryExit $ openPartialPath allFiles str

      tryExit $ openModule allFiles str

      tryExit $ openDef opts str
        [ Class
        , Constructor
        , Data
        , Newtype
        , Type
        , Variable Global
        ]

    (viewDef -> Just def) : [str] -> do
      tryExit $ openDef opts str [def]

    (viewModule -> True) : [str] -> do
      allFiles <- liftIO ls
      tryExit $ openModule allFiles str

    (viewTypes -> True) : [str] -> do
      tryExit $ openDef opts str
        [ Class
        , Data
        , Newtype
        , Type
        ]

    (viewValues -> True) : [str] -> do
      tryExit $ openDef opts str
        [ Constructor
        , Variable Global
        ]

    _ -> liftIO badArgsExit

  mapM_ putStrLn matchingFiles
  Exit.exitFailure

helpExit :: IO ()
helpExit = do
  mapM_ putStrLn
    [ "Usage:"
    , ""
    , "vimlaucher -h"
    , "vimlaucher --help"
    , "  Prints this help message."
    , ""
    , "vimlaucher EXACT_PATH"
    , "  Opens the file given by EXACT_PATH."
    , ""
    , "vimlaucher PARTIAL_PATH"
    , "  Opens the file given by PARTIAL_PATH."
    , ""
    , "vimlaucher module MODULE"
    , "vimlaucher MODULE"
    , "  Opens the file containing the Haskell module named MODULE."
    , ""
    , "vimlaucher OPTS KIND IDENTIFIER"
    , "vimlaucher OPTS IDENTIFIER"
    , "  Opens the file containing the definition named IDENTIFIER given by KIND if provided."
    , "  where"
    , "    OPTS is any of:"
    , "      -i : ignore case"
    , "       + : open at the line of the definition"
    , "    KIND is one of:"
    , "      class"
    , "      cons"
    , "      constructor"
    , "      data"
    , "      local"
    , "      new"
    , "      newtype"
    , "      type"
    , "      var"
    , "      variable"
    , ""
    , "If there are no candidate files, then this exits with exit code 1."
    , "If there are multiple candidate files, then this exits with exit code 1 and prints the candidate files to stdout."
    , "Otherwise this exits with exit code 0 and prints the arguments needed to open the file for vim's command line."
    ]
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

tryExit :: IO OpenResult -> Prog ()
tryExit action = liftIO action >>= \case
  Left files -> State.modify $ \st -> let
    stFiles  = stMatchingFiles st
    stFiles' = Set.union stFiles $ Set.fromList files
    in st { stMatchingFiles = stFiles' }
  Right args -> liftIO $ do
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

viewValues :: String -> Bool
viewValues = \case
  "values" -> True
  "vals" -> True
  _ -> False

viewTypes :: String -> Bool
viewTypes = \case
  "types" -> True
  _ -> False

viewModule :: String -> Bool
viewModule = \case
  "module" -> True
  _ -> False

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
  toGrepQuery :: Options -> String -> a -> Maybe String

instance ToGrepQuery DefType where
  toGrepQuery opts identifier = let
    kwDecl kwName = "^ *" ++ kwName ++ " \\+" ++ identifier ++ "\\>"
    globalVar name = pure $ "^" ++ name ++ " *\\(::\\|$\\)"
    in \case
      Class -> do
        guard $ isType opts identifier
        pure $ kwDecl "class"
      Constructor -> do
        guard $ isType opts identifier
        pure $ " [=|] *" ++ identifier ++ "\\>"
      Data -> do
        guard $ isType opts identifier
        pure $ kwDecl "data"
      Newtype -> do
        guard $ isType opts identifier
        pure $ kwDecl "newtype"
      Type -> do
        guard $ isType opts identifier
        pure $ kwDecl "type"
      Variable scope -> do
        guard $ isVariable opts identifier
        case scope of
          Global -> globalVar identifier
          Local -> globalVar $ " \\+" ++ identifier

instance ToGrepQuery [DefType] where
  toGrepQuery opts identifier defs = let
    toQuery = toGrepQuery opts identifier
    queries = Maybe.mapMaybe toQuery defs
    in case queries of
      [] -> Nothing
      _ -> Just $ "\\(" ++ List.intercalate "\\)\\|\\(" queries ++ "\\)"

isType :: Options -> String -> Bool
isType opts = \case
  c : _ -> List.all id
    [ Char.isAlpha c
    , Char.toUpper c == c || oIgnoreCase opts
    ]
  _ -> False

isVariable :: Options -> String -> Bool
isVariable opts = \case
  c : _ -> List.all id
    [ Char.isAlpha c || c == '_'
    , Char.toLower c == c || oIgnoreCase opts
    ]
  _ -> False

type OpenResult = Either [FilePath] VimArgs

openDef :: Options -> String -> [DefType] -> IO OpenResult
openDef opts name defs = case toGrepQuery opts name defs of
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

openModule :: [FilePath] -> String -> IO OpenResult
openModule allFiles str = case fromHaskellPath str of
  Nothing -> pure $ Left []
  Just str' -> openPartialPath allFiles str'

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

