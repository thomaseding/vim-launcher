{-# LANGUAGE LambdaCase #-}

module VimLauncher (
  main,
) where

import qualified Control.Exception as Exn
import           Control.Monad
import qualified Data.List as List
import           Prelude hiding (pred)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Process as Proc

main :: IO ()
main = Env.getArgs >>= \case

  [str] -> do
    try $ openExact str

    files <- ls

    try $ openPartialPath files str

    try $ case fromHaskellPath str of
      Nothing -> pure Nothing
      Just str' -> openPartialPath files str'

    try $ openDef str

    noVimArgs

  _ -> do
    badArgs

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

gitProc :: String -> [String] -> IO [String]
gitProc name args = fmap lines $ Proc.readProcess "git" (name : args) []

grep :: [String] -> IO [String]
grep args = do
  let action = gitProc "grep" $ "-n" : args
  Exn.catch action $ \e -> let
    _ = e :: Exn.SomeException
    in pure []

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

getSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
getSuccess = \case
  [] -> pure Nothing
  m : ms -> m >>= \case
    Nothing -> getSuccess ms
    x @ Just {} -> pure x

fromSingle :: [a] -> Maybe a
fromSingle = \case
  [x] -> Just x
  _ -> Nothing

openDef :: String -> IO (Maybe VimArgs)
openDef name = do
  let kwDecl kind = grep ["\\<" ++ kind ++ "\\s\\+" ++ name ++ "\\>"]
      kinds = ["data", "type", "newtype", "class"]
      funcDecl = grep ["^" ++ name ++ "\\s*::"]
      greps = funcDecl : map kwDecl kinds
  mResult <- getSuccess $ map (fmap fromSingle) greps
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

