{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
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
import qualified Brick.Widgets.List     as BrickList
import           Client
import           Control.Applicative    (liftA2)
import           Control.Monad          (guard, join, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Vector            as Vector
import           Graphics.Vty           (Attr, black, blue, bold, cyan, green,
                                         red, standout, underline, white,
                                         withStyle, withURL, yellow)
import qualified Graphics.Vty           as V
import           Lens.Micro.Platform
import           Network.URI            (URI)
import qualified Network.URI            as URI
import qualified Output

data Loadable a = NotStarted | Loading | Loaded a deriving (Show, Eq)

data State = State
  { _currentURI    :: Maybe URI
  , _geminiContent :: Loadable Client.GeminiResponse
  , _urlEditor     :: Editor String Name
  , _logs          :: [String]
  , _focusRing     :: Focus.FocusRing Name
  , _contentList   :: BrickList.List Name Line
  , _history       :: Vector.Vector URI
  } deriving (Show)

instance Show (Focus.FocusRing n) where
  show f = "<focus ring>"

data Name = UrlEdit | ContentViewport | ContentList deriving (Show, Eq, Ord)
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
myAttrMap = attrMap V.defAttr [ (BrickList.listSelectedAttr, V.black `on` V.white)
                              , ("geminiH1", V.currentAttr `withStyle` bold `withStyle` underline)
                              , ("geminiH2", V.currentAttr `withStyle` underline)
                              , ("geminiH3", V.currentAttr `withStyle` bold)
                              , ("geminiUri", fg yellow)
                              , ("geminiPre", fg blue)
                              -- , ("geminiQuote", fg white)
                              -- , ("geminiUl", fg white)
                              -- , (BrickList.listAttr, V.red `on` V.black)
                              ]

pop :: Vector.Vector a -> (Vector.Vector a, Maybe a)
pop v =
  if Vector.null v then
    (v, Nothing)
  else
    (Vector.init v, Just $ Vector.last v)

popHistory :: State -> Either State (State, URI)
popHistory s =
  case pop (s ^. history) of
    (newHistory, Just uri) ->
      Right (s & history .~ newHistory, uri)
    _ ->
      Left s

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
  let
    focus =
      Focus.focusGetCurrent $ s ^. focusRing

    cycleFocus =
      continue $ s & focusRing %~ Focus.focusNext
  in
    case (focus, e) of
      ((Just UrlEdit), (V.EvKey V.KEnter [])) ->
        let
          editContents =
            s ^. urlEditor & Editor.getEditContents & head
        in
          case URI.parseURI editContents of
            Just uri -> do
              let s' = s & focusRing %~ Focus.focusNext
              liftIO (doFetch uri True s') >>= continue
            Nothing ->
              continue s
      (_, (V.EvKey (V.KChar '\t') [])) ->
        cycleFocus
      (_, (V.EvKey (V.KChar 'c') [V.MCtrl])) ->
        halt s
      ((Just UrlEdit), ev) -> do
        newState <- Editor.handleEditorEvent e (s ^. urlEditor)
        continue $ set urlEditor newState s
      ((Just ContentViewport), (V.EvKey V.KBS [])) -> do
        case popHistory s of
          Right (s, uri) ->
              liftIO (doFetch uri False s) >>= continue
          Left s ->
            continue s
      ((Just ContentViewport), (V.EvKey V.KEnter [])) -> do
        let selection = s ^. contentList & BrickList.listSelectedElement
        case selection of
          Just (n, LinkLine (Right uri) desc) ->
            liftIO (doFetch uri True s) >>= continue
          _ ->
            continue s
      ((Just ContentViewport), ev) -> do
        newState <- BrickList.handleListEventVi BrickList.handleListEvent e (s ^. contentList)
        continue $ set contentList newState s
      _ ->
        continue s

doFetch :: URI -> Bool -> State -> IO State
doFetch uri pushHistory s = do
  -- TODO: do this asynchronously
  maybe s (responseToState s pushHistory ) <$> Client.get uri

makeUrlEditor :: String -> Editor String Name
makeUrlEditor =
  Editor.editor UrlEdit (Just 1)

responseToState :: State -> Bool -> GeminiResponse -> State
responseToState s pushHistory geminiResponse@(Client.GeminiResponse uri responseData) =
  let lines =
        case responseData of
          Success (GeminiContent (GeminiPage {pageLines})) ->
            pageLines
          Success (UnknownContent mimeType text) ->
            -- [TextLine ("Do I look like I know what a '" <> mimeType <> "' is?")]
            fmap TextLine (T.lines text)
          other ->
            [ TextLine "Something wacky is happening!"
            , TextLine $ T.pack (show other)
            ]

      maybeOldURI =
        s ^. currentURI
  in
    s & geminiContent .~ Loaded geminiResponse
      & currentURI ?~ uri
      & urlEditor .~ makeUrlEditor (URI.uriToString id uri "")
      & contentList %~ BrickList.listReplace (Vector.fromList lines) (Just 0)
      & history %~ (\v ->
                      case (pushHistory, maybeOldURI) of
                       (True, Just oldUri) -> Vector.snoc v oldUri
                       _                   -> v
                   )

drawUI :: State -> [Widget Name]
drawUI s =
  let
    hasFocus a b =
      a == Just b

    focus =
      Focus.focusGetCurrent $ s ^. focusRing

    drawUrlEditor =
      Editor.renderEditor (str . unlines) (hasFocus focus UrlEdit) (s ^. urlEditor)

    drawContent =
      vBox ([ s ^. geminiContent
               & drawLoadable (drawGeminiContent
                               (hasFocus focus ContentViewport)
                               (s ^. contentList))
           ]
            ++ [ fill ' ' | not $ loadableReady (s ^. geminiContent)])
  in
    [ center $ border $
       vBox [ drawUrlEditor
            , hBorder
            , drawContent
            , hBorder
            , drawStatusLine s
            ]
    ]

loadableReady :: Loadable a -> Bool
loadableReady loadable =
  case loadable of
    NotStarted -> False
    Loading    -> False
    Loaded _   -> True

drawLoadable :: (a -> Widget Name) -> Loadable a -> Widget Name
drawLoadable draw loadable =
  case loadable of
    NotStarted -> str "Not Started"
    Loading    -> str "Loading..."
    Loaded a   -> draw a

drawGeminiContent :: Bool -> BrickList.List Name Line -> GeminiResponse -> Widget Name
drawGeminiContent focused lineList response =
  case response of
    GeminiResponse uri (Success _) ->
        BrickList.renderList (const (\line -> displayLine line <+> str " ")) focused lineList
    GeminiResponse uri resp ->
      str "Unknown response!" <=> (str $ show resp)

displayLine :: Line -> Widget Name
displayLine line =
  -- tabs screw up my terminal
  case mapLine tabsToSpaces line of
    HeadingLine H1 t  -> withAttr "geminiH1" $ txt t
    HeadingLine H2 t  -> withAttr "geminiH2" $ txt t
    HeadingLine H3 t  -> withAttr "geminiH3" $ txt t
    TextLine t        -> withAttr "geminiText" $ txtWrap t
    ULLine t          -> withAttr "geminiUl" $ txt ("• " <> t)
    PreLine t         -> withAttr "geminiPre" $ txt t
    QuoteLine t       -> withAttr "geminiQuote" $ txt ("┆ " <> t)
    LinkLine uri desc -> displayLink uri desc

tabsToSpaces :: Text -> Text
tabsToSpaces =
  T.replace "\t" "  "

mapLine :: (Text -> Text) -> Line -> Line
mapLine fn = \case
  HeadingLine h t -> HeadingLine h (fn t)
  TextLine t -> TextLine (fn t)
  ULLine t -> ULLine (fn t)
  PreLine t -> PreLine (fn t)
  QuoteLine t -> QuoteLine (fn t)
  LinkLine uri desc -> LinkLine uri (fn desc)

displayLink :: Either Text URI -> Text -> Widget Name
displayLink uriOrErr desc =
  let
    uriBit =
      case uriOrErr of
          Right uri ->
            str "(" <+> (showUri uri) <+> str ")"

          Left  foo ->
            txt ("<URI parse failed!> - " <> foo)

    uriToTxt uri =
      T.pack $ URI.uriToString id uri ""

    showUri uri =
      hyperlink (uriToTxt uri)
        $ withAttr "geminiUri"
        $ txt (uriToTxt uri)
  in
    txt desc <+> str " " <+> uriBit

drawStatusLine :: State -> Widget Name
drawStatusLine s =
  let
    hist =
      s ^. history & Vector.toList

    drawHistory =
      case popHistory s of
        Left _ ->
          str "No history"
        Right (_, lastPage) ->
          str ("(" <> (show $ length hist) <> ") Last page: " <> (show lastPage) <> " (backspace)")

    drawMime =
      str $ (getMimeType s) <> " "

    help =
      str "(Ctrl+c to quit)"
  in
    padRight Max drawHistory <+> drawMime <+> help

getMimeType :: State -> String
getMimeType s = case s ^. geminiContent of
  Loaded (GeminiResponse uri (Success (UnknownContent mimeType txt))) ->
    T.unpack mimeType
  _ -> "?"


initState :: State
initState = State
  { _currentURI = Nothing
  , _geminiContent = NotStarted
  , _urlEditor = makeUrlEditor "gemini://gemini.circumlunar.space/"
  , _focusRing = Focus.focusRing [UrlEdit, ContentViewport]
  , _contentList = BrickList.list ContentList Vector.empty 1
  , _logs = []
  , _history = Vector.empty
  }

main :: IO ()
main = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initState
