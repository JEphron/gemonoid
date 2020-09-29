{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Brick
import qualified Brick
import qualified Brick.Main             as Brick
import           Brick.Util             (on)
import qualified Brick.Widgets.Border   as B
import qualified Brick.Widgets.Center   as C
import qualified Brick.Widgets.Dialog   as Dialog
import           Brick.Widgets.Edit     (Editor)
import qualified Brick.Widgets.Edit     as Editor
import           Client
import           Control.Monad          (join, void)
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
  } deriving (Show)

data Name = UrlEdit | ContentVP deriving (Show, Eq, Ord)
type Event = ()

app :: App State Event Name
app = App { appDraw = drawUI
          , appChooseCursor = \_ -> showCursorNamed UrlEdit
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
handleEvent s (VtyEvent (V.EvKey (V.KEnter) [])) = liftIO (doFetch s) >>= continue
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent ev) =
  Editor.handleEditorEvent ev (_urlEditor s) >>= (\it -> continue $ s {_urlEditor = it})

doFetch :: State -> IO State
doFetch s = do
  -- TODO: do this asynchronously
  let editContents = head $ Editor.getEditContents $ _urlEditor s
  case URI.parseURI editContents of
    Just uri ->
      responseToState s <$> Client.get uri
    Nothing ->
      return s

responseToState :: State -> Maybe GeminiResponse -> State
responseToState s (Just geminiResponse@(Client.GeminiResponse uri _)) =
  s { _geminiContent = Loaded geminiResponse, _currentURI = Just uri}
responseToState s Nothing = s

drawUI :: State -> [Widget Name]
drawUI s =
  [
    C.center $ B.border $
      vBox [urlEditor, B.hBorder, outputVp, B.hBorder, drawStatusLine s]
  ]
  where urlEditor = (Editor.renderEditor (str . unlines) True (_urlEditor s))
        outputVp = viewport ContentVP Vertical $
          drawLoadable (_geminiContent s) drawGeminiContent

drawLoadable :: Loadable a -> (a -> Widget Name) -> Widget Name
drawLoadable NotStarted _    = str "Not Started"
drawLoadable Loading _       = str "Loading..."
drawLoadable (Loaded a) draw = draw a

drawGeminiContent :: GeminiResponse -> Widget Name
drawGeminiContent (GeminiResponse uri (Success (GeminiContent GeminiPage {pageLines}))) =
  vBox (map displayLine pageLines)
drawGeminiContent (GeminiResponse uri resp) = str (show resp)

displayLine :: Line -> Widget Name
displayLine (HeadingLine H1 t)  = withAttr "geminiH1" $ txt t
displayLine (HeadingLine H2 t)  = withAttr "geminiH2" $ txt t
displayLine (HeadingLine H3 t)  = withAttr "geminiH3" $ txt t
displayLine (TextLine t)        = withAttr "geminiText" $ txt t
displayLine (ULLine t)          = withAttr "geminiUl" $ txt ("• " <> t)
displayLine (PreLine t)         = withAttr "geminiPre" $ txt t
displayLine (QuoteLine t)       = withAttr "geminiQuote" $ txt ("┆ " <> t)
displayLine (LinkLine uri desc) = displayLink uri desc

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
debugGeminiContent s = _geminiContent s & show


initState :: State
initState = State
  { _currentURI = Nothing
  , _geminiContent = NotStarted
  , _urlEditor = Editor.editor UrlEdit (Just 1) "gemini://gemini.circumlunar.space/"
  , _logs = []
  }

main :: IO ()
main = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initState
