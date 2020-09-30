{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Brick
import qualified Brick
import qualified Brick.Main             as Brick
import           Brick.Util             (on)

import qualified Brick.Focus            as Focus
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import qualified Brick.Widgets.Dialog   as Dialog
import           Brick.Widgets.Edit     (Editor)
import qualified Brick.Widgets.Edit     as Editor
import           Client
import           Control.Applicative    (liftA2)
import           Control.Monad          (guard, join, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Graphics.Vty           (Attr, black, blue, bold, cyan, green,
                                         red, standout, underline, white,
                                         withStyle, withURL, yellow)
import qualified Graphics.Vty           as V
import           Lens.Micro.Platform
import           Network.URI            (URI)
import qualified Network.URI            as URI
import qualified Output

data Loadable a = NotStarted | Loading | Loaded a deriving (Show)

data State = State
  { _currentURI    :: Maybe URI
  , _geminiContent :: Loadable Client.GeminiResponse
  , _urlEditor     :: Editor String Name
  , _logs          :: [String]
  , _focusRing     :: Focus.FocusRing Name
  } deriving (Show)

instance Show (Focus.FocusRing n) where
  show f = "<focus ring>"

data Name = UrlEdit | ContentViewport deriving (Show, Eq, Ord)
type Event = ()

makeLenses ''State

app :: App State Event Name
app = App { appDraw = drawUI
          , appChooseCursor = Focus.focusRingCursor (^. focusRing)
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const myAttrMap
          }

myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr [ ("geminiH1", fg white `withStyle` bold `withStyle` underline)
                              , ("geminiH2", fg white `withStyle` underline)
                              , ("geminiH3", fg white `withStyle` bold)
                              , ("geminiQuote", fg white)
                              , ("geminiUri", fg yellow)
                              , ("geminiPre", fg blue)
                              , ("geminiUl", fg white)
                              ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
  let
    focus = Focus.focusGetCurrent $ s ^. focusRing
    cycleFocus = continue $ s & focusRing %~ Focus.focusNext
  in
    case (focus, e) of
      ((Just UrlEdit), (V.EvKey (V.KEnter) [])) ->
        liftIO (doFetch s) >>= continue
      (_, (V.EvKey (V.KChar '\t') [])) ->
        cycleFocus
      (_, (V.EvKey (V.KChar 'c') [V.MCtrl])) ->
        halt s
      ((Just UrlEdit), ev) -> do
        newState <- Editor.handleEditorEvent e (s ^. urlEditor)
        continue $ set urlEditor newState s
      _ ->
        continue s

doFetch :: State -> IO State
doFetch s = do
  -- TODO: do this asynchronously
  let editContents = s ^. urlEditor & Editor.getEditContents & head
  case URI.parseURI editContents of
    Just uri -> do
      maybe s (responseToState s) <$> Client.get uri
    Nothing ->
      return s

responseToState :: State -> GeminiResponse -> State
responseToState s geminiResponse@(Client.GeminiResponse uri _) =
    s & geminiContent .~ Loaded geminiResponse
      & currentURI ?~ uri

drawUI :: State -> [Widget Name]
drawUI s =
  let
    focus =
      Focus.focusGetCurrent $ s ^. focusRing
    drawUrlEditor =
      Editor.renderEditor (str . unlines) (focus == (Just UrlEdit)) (s ^. urlEditor)
    outputVp =
      viewport ContentViewport Vertical $
      s ^. geminiContent & drawLoadable (drawGeminiContent (focus == Just ContentViewport))
  in
    [ center $ border $
       vBox [drawUrlEditor, hBorder, outputVp, hBorder, drawStatusLine s]
    ]

drawLoadable :: (a -> Widget Name) -> Loadable a -> Widget Name
drawLoadable draw loadable =
  case loadable of
    NotStarted -> str "Not Started"
    Loading    -> str "Loading..."
    Loaded a   -> draw a

drawGeminiContent :: Bool -> GeminiResponse -> Widget Name
drawGeminiContent focused (GeminiResponse uri (Success (GeminiContent GeminiPage {pageLines}))) =
  vBox (map displayLine pageLines)
drawGeminiContent focused (GeminiResponse uri resp) =
  str (show resp)

displayLine :: Line -> Widget Name
displayLine line =
  case line of
    (HeadingLine H1 t)  -> withAttr "geminiH1" $ txt t
    (HeadingLine H2 t)  -> withAttr "geminiH2" $ txt t
    (HeadingLine H3 t)  -> withAttr "geminiH3" $ txt t
    (TextLine t)        -> withAttr "geminiText" $ txt t
    (ULLine t)          -> withAttr "geminiUl" $ txt ("• " <> t)
    (PreLine t)         -> withAttr "geminiPre" $ txt t
    (QuoteLine t)       -> withAttr "geminiQuote" $ txt ("┆ " <> t)
    (LinkLine uri desc) -> displayLink uri desc

displayLink :: Either Text URI -> Text -> Widget Name
displayLink uriOrErr desc =
  txt desc <+> str " " <+> uriBit
  where uriBit = case uriOrErr of
                    Right uri -> str "(" <+> (showUri uri) <+> str ")"
                    Left  foo -> txt ("<URI parse failed!> - " <> foo)
        uriToTxt uri = T.pack $ URI.uriToString id uri ""
        showUri uri = modifyDefAttr (`withURL` (uriToTxt uri)) $
                         withAttr "geminiUri" $ txt (uriToTxt uri)

drawStatusLine :: State -> Widget Name
drawStatusLine s = padRight Max status <+> help
  where status = case _currentURI s of
          Just uri -> str ("Currently at " <> show uri)
          Nothing  -> str "Nowhere ;("
        help = str "(Ctrl+c to quit)"

debugGeminiContent :: State -> String
debugGeminiContent s = s ^. geminiContent & show

initState :: State
initState = State
  { _currentURI = Nothing
  , _geminiContent = NotStarted
  , _urlEditor = Editor.editor UrlEdit (Just 1) "gemini://gemini.circumlunar.space/"
  , _focusRing = Focus.focusRing [UrlEdit, ContentViewport]
  , _logs = []
  }

main :: IO ()
main = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initState
