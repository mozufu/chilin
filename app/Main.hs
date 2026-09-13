module Main (main) where

import Chilin.Repository (bootstrapAdmin, openEnv)
import Chilin.Server (serve)
import Chilin.Transport (runSSH)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  result <- try (run args) :: IO (Either SomeException ())
  either (die . displayException) pure result

run :: [String] -> IO ()
run ["--help"] = putStr usage
run ["--version"] = putStrLn "chilin 0.1.0.0"
run ("init" : args) = do
  options <- parseOptions ["--root", "--admin"] args
  let root = option "--root" "data" options
      admin = option "--admin" "admin" options
  token <- requiredEnv "CHILIN_ADMIN_TOKEN"
  env <- openEnv root
  bootstrapAdmin env (T.pack admin) (T.pack token)
  putStrLn ("Initialized Chilin storage for " <> admin)
run ("serve" : args) = do
  options <- parseOptions ["--root", "--host", "--port"] args
  let root = option "--root" "data" options
      host = option "--host" "127.0.0.1" options
  port <- maybe (die "--port must be an integer between 1 and 65535") pure (readMaybe (option "--port" "8080" options))
  unless (port > 0 && port <= 65535) $ die "--port must be between 1 and 65535"
  env <- openEnv root
  putStrLn ("Chilin listening on " <> host <> ":" <> show port)
  serve env host port
run ("ssh" : args) = do
  options <- parseOptions ["--root"] args
  token <- requiredEnv "CHILIN_TOKEN"
  command <- requiredEnv "SSH_ORIGINAL_COMMAND"
  env <- openEnv (option "--root" "data" options)
  runSSH env (T.pack token) (T.pack command)
run _ = die usage

parseOptions :: [String] -> [String] -> IO [(String, String)]
parseOptions allowed = go []
 where
  go accumulated [] = pure accumulated
  go accumulated (key : value : rest)
    | key `elem` allowed && key `notElem` map fst accumulated = go ((key, value) : accumulated) rest
  go _ _ = die "Unknown, duplicate, or incomplete option; use chilin --help"

option :: String -> String -> [(String, String)] -> String
option key fallback = fromMaybe fallback . lookup key

requiredEnv :: String -> IO String
requiredEnv name = lookupEnv name >>= maybe (die ("Missing environment variable " <> name)) pure

usage :: String
usage =
  unlines
    [ "chilin — Git hosting with a myque collaboration store"
    , ""
    , "chilin init [--root data] [--admin admin]"
    , "  Reads a >=32-character administrator token from CHILIN_ADMIN_TOKEN."
    , "chilin serve [--root data] [--host 127.0.0.1] [--port 8080]"
    , "chilin ssh [--root data]"
    , "  Restricted SSH forced command; CHILIN_TOKEN and SSH_ORIGINAL_COMMAND required."
    , ""
    , "Git: /owner/repo.git and read-only /owner/repo.tracker.git"
    , "API: /api/repos; authenticate using Bearer token or HTTP Basic password token."
    , "Tracker mutations require If-Match: \"<revision>\" and Idempotency-Key headers."
    , "Bind behind a TLS reverse proxy before exposing credentials over a network."
    ]
