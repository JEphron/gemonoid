{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Output where

import Client
import qualified Client
import Data.Maybe
import qualified Data.Text.IO as TIO
import qualified Network.URI as URI
import qualified System.Console.ANSI as ANSI

linkDescriptionStyle = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]

linkUrlStyle =
  [ ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow,
    ANSI.SetUnderlining ANSI.SingleUnderline
  ]

preStyle = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]

hStyle h =
  [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
    ++ [ANSI.SetUnderlining ANSI.SingleUnderline | h == H1]
    ++ [ANSI.SetItalicized True | h == H2]

ulStyle = [ANSI.SetConsoleIntensity ANSI.BoldIntensity]

errorStyle = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]

withStyle :: [ANSI.SGR] -> IO () -> IO ()
withStyle style fn = do
  ANSI.setSGR style
  fn
  ANSI.setSGR [ANSI.Reset]

displayLine :: Line -> IO ()
displayLine (TextLine t) = TIO.putStrLn t
displayLine (LinkLine uriOrStr description) = do
  withStyle linkDescriptionStyle $ do
    TIO.putStr description
  putStr " ("
  withStyle linkUrlStyle $ do
    case uriOrStr of
      Right uri ->
        putStr $ URI.uriToString id uri ""
      Left str -> do
        TIO.putStr "<parse failed!> "
        TIO.putStr str
  putStr ")"
  TIO.putStr "\n"
displayLine (PreLine t) = withStyle preStyle $ do
  TIO.putStrLn t
displayLine (HeadingLine h t) = do
  withStyle (hStyle h) $ do
    TIO.putStrLn t
displayLine (ULLine t) = do
  withStyle ulStyle $ do
    TIO.putStrLn ("• " <> t)
displayLine (QuoteLine t) = do
  TIO.putStrLn ("┆ " <> t)

display :: GeminiResponse -> IO ()
display (GeminiResponse _ (Success (GeminiContent GeminiPage {pageLines}))) =
  mapM_ displayLine pageLines
display x =
  print x

getAndDisplay :: String -> IO ()
getAndDisplay string =
  case URI.parseURI string of
    Just uri -> Client.get uri >>= mapM_ display
    Nothing -> withStyle errorStyle $ TIO.putStrLn "error, invalid URI"
