{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Brick
import qualified Brick
import qualified Brick.Main             as Brick
import           Brick.Util             (on)
import qualified Data.Char              as Char
import           Data.List
import           Data.String            (fromString)
import           Raw

import qualified Brick.Focus            as Focus
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import qualified Brick.Widgets.Dialog   as Dialog
import           Brick.Widgets.Edit     (DecodeUtf8, Editor)
import qualified Brick.Widgets.Edit     as Editor
import qualified Brick.Widgets.List     as BrickList
import           Client
import           Control.Applicative    (liftA2)
import           Control.Monad          (guard, join, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Zipper       as Zipper
import qualified Data.Vector            as Vector
import           Graphics.Vty           (Attr, black, blue, bold, cyan, green,
                                         magenta, red, standout, underline,
                                         white, withStyle, withURL, yellow)
import qualified Graphics.Vty           as V
import           Lens.Micro.Platform
import           Network.URI            (URI)
import qualified Network.URI            as URI
import qualified Output

data Loadable a = NotStarted | Loading | Loaded a deriving (Show, Eq)

data State = State
  { _currentURI    :: Maybe URI
  , _geminiContent :: Loadable Client.GeminiResponse
  , _urlBar        :: Editor String Name
  , _logs          :: [String]
  , _focusRing     :: Focus.FocusRing Name
  , _contentList   :: BrickList.List Name Line
  , _history       :: Vector.Vector URI
  , _promptDialog  :: Dialog.Dialog (Maybe Text)
  , _textPrompt    :: Editor String Name
  } deriving (Show)

instance Show (Focus.FocusRing n) where
  show f = "<focus ring>"

instance Show (Dialog.Dialog n) where
  show f = "<dialog>"

data Name = UrlBar | ContentViewport | TextPrompt | ContentList deriving (Show, Eq, Ord)
type Event = ()

makeLenses ''State

app :: App State Event Name
app = App { appDraw = drawUI
          , appChooseCursor = Focus.focusRingCursor (^. focusRing)
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const myAttrMap
          }

banner :: String
banner = [rw|
  ____  ___ __  __  ___  _  _  ___ ___ ____
 / ___|| __|  \/  |/ _ \| \| |/ _ \_ _|  _ \
| |  _ | _|| |\/| | (_) | .` | (_) | || | | |
| |_| ||___|_|  |_|\___/|_|\_|\___/___| |_| |
 \____|                               |____/  |]


rainbow =
  let toAttr i color =
        (fromString ("rainbow" <> show i), fg color)
  in
    imap toAttr [red, green, blue, yellow, magenta, cyan]

myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr ([ (BrickList.listSelectedAttr, V.black `on` V.white)
                               , ("geminiH1", V.currentAttr `withStyle` bold `withStyle` underline)
                               , ("geminiH2", V.currentAttr `withStyle` underline)
                               , ("geminiH3", V.currentAttr `withStyle` bold)
                               , ("geminiUri", fg yellow)
                               , ("yellow", fg yellow)
                               , ("geminiPre", fg blue)
                               , (Dialog.buttonAttr, fg white)
                               , (Dialog.buttonSelectedAttr, black `on` yellow)
                               ] ++ rainbow)

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
    --focusOn name =
    --  s & focusRing %~ Focus.focusNext
  in
    case (focus, e) of
      (Just UrlBar, V.EvKey V.KEnter []) ->
        let
          editContents =
            s ^. urlBar & Editor.getEditContents & head
        in
          case URI.parseURI editContents of
            Just uri -> do
              liftIO (doFetch uri True s) >>= continue
            Nothing ->
              continue s


      (Just UrlBar, V.EvKey (V.KChar '\t') []) ->
        cycleFocus

      (Just ContentViewport, V.EvKey (V.KChar '\t') []) ->
        cycleFocus

      (_, V.EvKey (V.KChar 'c') [V.MCtrl]) ->
        halt s

      (Just UrlBar, V.EvKey (V.KChar 'f') [V.MCtrl]) ->
        continue $ s & urlBar %~ Editor.applyEdit Zipper.moveRight

      (Just UrlBar, V.EvKey (V.KChar 'b') [V.MCtrl]) ->
        continue $ s & urlBar %~ Editor.applyEdit Zipper.moveLeft

      (Just UrlBar, ev) ->
        continue =<<
          handleEventLensed s urlBar handleEditorEvent e


      (Just TextPrompt, V.EvKey V.KEnter []) -> do
        case (s ^. currentURI, Dialog.dialogSelection (s ^. promptDialog)) of
          (Just currentUri, Just (Just _)) ->
            let
              query = s ^. textPrompt & Editor.getEditContents & unlines & T.pack & T.strip & T.unpack
              newUri = currentUri{URI.uriQuery=("?"<>(URI.escapeURIString URI.isAllowedInURI $ query))}
            in
              liftIO (doFetch newUri True s) >>= continue

          (Just currentUri, Just Nothing) ->
            doPopHistory s

          (Just currentUri, Nothing) ->
            continue s

          (Nothing, _) ->
            error "wut."

      (Just TextPrompt, ev) -> do
        s' <- handleEventLensed s promptDialog Dialog.handleDialogEvent e
        s'' <- handleEventLensed s' textPrompt handleEditorEvent e
        continue s''

      (Just ContentViewport, V.EvKey V.KEnter []) -> do
        let selection = s ^. contentList & BrickList.listSelectedElement
        case selection of
          Just (n, LinkLine (Right uri) desc) ->
            liftIO (doFetch uri True s) >>= continue
          _ ->
            continue s

      (Just ContentViewport, V.EvKey V.KBS []) ->
        doPopHistory s

      (Just ContentViewport, ev) ->
          continue =<<
            handleEventLensed s contentList handleListEvent e

      _ ->
        continue s

handleEditorEvent :: (DecodeUtf8 t, Eq t, Monoid t) => V.Event -> Editor t n -> EventM n (Editor t n)
handleEditorEvent ev ed =
  -- decorate with some emacsy keybindings
  case ev of
      V.EvKey (V.KChar 'f') [V.MCtrl] ->
        return $ Editor.applyEdit Zipper.moveRight ed

      V.EvKey (V.KChar 'b') [V.MCtrl] ->
        return $ Editor.applyEdit Zipper.moveLeft ed

      _ ->
        Editor.handleEditorEvent ev ed

doPopHistory :: State -> EventM Name (Next State)
doPopHistory s =
    case popHistory s of
        Right (s, uri) ->
            liftIO (doFetch uri False s) >>= continue
        Left s ->
            continue s

textPromptActive :: State -> Bool
textPromptActive s =
  case s ^. geminiContent of
    Loaded (GeminiResponse _ (Input _)) ->
      True
    _ -> False

handleListEvent =
  BrickList.handleListEventVi BrickList.handleListEvent

doFetch :: URI -> Bool -> State -> IO State
doFetch uri pushHistory s = do
  -- TODO: do this asynchronously
  maybe s (responseToState s pushHistory ) <$> Client.get uri

makeUrlEditor :: String -> Editor String Name
makeUrlEditor =
  Editor.editor UrlBar (Just 1)

responseToState :: State -> Bool -> GeminiResponse -> State
responseToState s shouldPushHistory geminiResponse@(Client.GeminiResponse uri responseData) =
  let
    pushHistory v =
      case (shouldPushHistory, s ^. currentURI) of
        (True, Just oldUri) -> Vector.snoc v oldUri
        _                   -> v
  in
  case responseData of
    Input promptText ->
      s & textPrompt .~ Editor.editor TextPrompt (Just 1) ""
        & geminiContent .~ Loaded geminiResponse
        & currentURI ?~ uri
        & focusRing .~ (Focus.focusSetCurrent TextPrompt focusRingPrompt)
        & history %~ pushHistory
        & promptDialog .~ Dialog.dialog
            (Just (T.unpack promptText))
            (Just (1, [("Back", Nothing), ("Submit", Just promptText)]))
            32

    Success content ->
        let lines =
                case content of
                  GeminiContent (GeminiPage {pageLines}) ->
                    pageLines

                  UnknownContent mimeType text ->
                    fmap TextLine (T.lines text)

        in
            s & geminiContent .~ Loaded geminiResponse
              & currentURI ?~ uri
              & urlBar .~ makeUrlEditor (URI.uriToString id uri "")
              & contentList %~ BrickList.listReplace (Vector.fromList lines) (Just 0)
              & focusRing .~ (Focus.focusSetCurrent ContentViewport focusRingNormal)
              & history %~ pushHistory
    other ->
      -- todo: fixme
      let bogusMessage =
            [ TextLine "welp..."
            , TextLine (T.pack $ show other)
            ]
      in
        s & geminiContent .~ Loaded geminiResponse
            & currentURI ?~ uri
            & urlBar .~ makeUrlEditor (URI.uriToString id uri "")
            & contentList %~ BrickList.listReplace (Vector.fromList bogusMessage) (Just 0)
            & focusRing .~ (preservingFocus s focusRingNormal)
            & history %~ pushHistory

getFocus :: State -> Maybe Name
getFocus s =
  Focus.focusGetCurrent $ s ^. focusRing

hasFocus :: State -> Name -> Bool
hasFocus s n =
    getFocus s == Just n

preservingFocus s newFocusRing =
  case getFocus s of
    Just currentFocus ->
      Focus.focusSetCurrent currentFocus newFocusRing

    Nothing ->
      newFocusRing

focusRingNormal =
  Focus.focusRing [UrlBar, ContentViewport]

focusRingPrompt =
  Focus.focusRing [UrlBar, TextPrompt]

drawUI :: State -> [Widget Name]
drawUI s =
  let
    drawUrlEditor =
      Editor.renderEditor
        (str . unlines)
        (hasFocus s UrlBar)
        (s ^. urlBar)

    drawContent =
      vBox ([ s ^. geminiContent & drawLoadable (drawGeminiContent s)]
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
    NotStarted -> drawStart
    Loading    -> str "Loading..."
    Loaded a   -> draw a

imap :: (Int -> a -> b) -> [a] -> [b]
imap fn xs  =
  map (uncurry fn) $ zip [0..] xs

drawStart =
  let
    makeAttrName i =
      fromString $ ("rainbow" ++ show (i `mod` (length rainbow)))

    drawBannerLine i string =
      hBox $ imap (\j char -> withAttr (makeAttrName (i + j)) $ str [char]) string

    drawBanner =
      vBox $ imap drawBannerLine (lines banner)

    drawLabel =
      str "press " <+> (yellowStr "<return>") <+> str " to start"
  in
   center $ borderWithLabel drawLabel $ drawBanner

yellowStr =
  withAttr "yellow" . str

drawGeminiContent :: State -> GeminiResponse -> Widget Name
drawGeminiContent s (GeminiResponse uri response) =
  case response of
    Success _ ->
        BrickList.renderList
          (\highlighted line -> displayLine line <+> str " ")
          (hasFocus s ContentViewport)
          (s ^. contentList)

    Input inputStr ->
        vBox [ Dialog.renderDialog
               ( s ^. promptDialog)
               (drawInputEdit s)
             , fill ' '
             ]

    resp ->
      str "Unknown response!" <=> (str $ show resp)

drawInputEdit s =
  border
  $ Editor.renderEditor
        (str . unlines)
        (hasFocus s TextPrompt)
        (s ^. textPrompt)

displayLine :: Line -> Widget Name
displayLine line =
  case mapLine cleanupLine line of
    HeadingLine H1 t  -> withAttr "geminiH1" $ txt t
    HeadingLine H2 t  -> withAttr "geminiH2" $ txt t
    HeadingLine H3 t  -> withAttr "geminiH3" $ txt t
    TextLine t        -> withAttr "geminiText" $ txtWrap t
    ULLine t          -> withAttr "geminiUl" $ txt ("• " <> t)
    PreLine t         -> withAttr "geminiPre" $ txt t
    QuoteLine t       -> withAttr "geminiQuote" $ txtWrap ("> " <> t)
    LinkLine uri desc -> displayLink uri desc

cleanupLine :: Text -> Text
cleanupLine t =
  -- tabs and CRs screw up Brick's text wrapping
  T.replace "\t" "  " t & T.filter Char.isPrint  & T.stripEnd

mapLine :: (Text -> Text) -> Line -> Line
mapLine fn = \case
  HeadingLine h t   -> HeadingLine h (fn t)
  TextLine t        -> TextLine (fn t)
  ULLine t          -> ULLine (fn t)
  PreLine t         -> PreLine (fn t)
  QuoteLine t       -> QuoteLine (fn t)
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
          str $ "("
                <> (show $ length hist)
                <> ") Last page: "
                <> (show lastPage)
                <> " (backspace)"

    drawMime =
      str $ (getMimeType s) <> " "

    drawDialogSel =
      str $ show (Dialog.dialogSelection (s ^. promptDialog))

    help =
      str "(Ctrl+c to quit)"
  in
    padRight Max drawHistory <+> drawDialogSel <+> help

getMimeType :: State -> String
getMimeType s = case s ^. geminiContent of
  Loaded (GeminiResponse uri (Success (UnknownContent mimeType txt))) ->
    T.unpack mimeType
  _ -> "?"


initState :: String -> State
initState initUri = State
  { _currentURI = Nothing
  , _geminiContent = NotStarted
  , _urlBar = makeUrlEditor initUri
  , _focusRing = focusRingNormal
  , _contentList = BrickList.list ContentList Vector.empty 1
  , _logs = []
  , _history = Vector.empty
  , _promptDialog = Dialog.dialog Nothing Nothing 99
  , _textPrompt = Editor.editor TextPrompt (Just 1) ""
  }

main :: IO ()
main = runApp "gemini://gemini.circumlunar.space/"

runApp :: String -> IO ()
runApp initUri = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app (initState initUri)
