module Log where

import           System.Console.ANSI

logf :: [SGR] -> String -> IO ()
logf sgr str = do
  setSGR sgr
  putStrLn str
  setSGR [Reset]

success :: String -> IO ()
success = logf [SetColor Foreground Vivid Green]

info :: String -> IO ()
info = logf [SetColor Foreground Vivid Blue]

error :: String -> IO ()
error = logf [SetColor Foreground Vivid Red]

warn :: String -> IO ()
warn = logf [SetColor Foreground Vivid Yellow]
