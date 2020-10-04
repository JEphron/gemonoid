{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Banner
import Brick
import qualified Brick
import qualified Brick.BChan as BChan
import qualified Brick.Focus as Focus
import qualified Brick.Main as Brick
import Brick.Util (on)
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.Dialog as Dialog
import Brick.Widgets.Edit (DecodeUtf8, Editor)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as BrickList
import Cache (Cache)
import qualified Cache
import Client
import Control.Applicative (liftA2)
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import Control.Monad (foldM, foldM_, forever, guard, join, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Char as Char
import Data.CircularList (CList)
import qualified Data.CircularList as CList
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graphics.Vty
  ( Attr,
    black,
    blue,
    bold,
    cyan,
    green,
    magenta,
    red,
    standout,
    underline,
    white,
    withStyle,
    withURL,
    yellow,
  )
import qualified Graphics.Vty as V
import History (History)
import qualified History
import Lens.Micro.Platform
import Loadable (Loadable (..))
import qualified Loadable
import Network.URI (URI)
import qualified Network.URI as URI
import Page (Page)
import qualified Page
import RequestProcessor
import System.Timeout
import Tab (Tab)
import qualified Tab
import Utils

data State = State
  { -- data
    _tabs :: CList Tab,
    _cache :: Cache,
    -- concurrency
    _requestChan :: TChan PageRequest,
    _killChan :: TChan KillRequest,
    -- ui
    _focusRing :: Focus.FocusRing Name,
    _urlBar :: Editor String Name,
    _contentList :: BrickList.List Name Line,
    _promptDialog :: Dialog.Dialog (Maybe Text),
    _textPrompt :: Editor String Name
  }
  deriving (Show)

instance Show (Focus.FocusRing n) where
  show f = "<focusring>"

instance Show (TChan n) where
  show f = "<tchan>"

instance Show (Dialog.Dialog n) where
  show f = "<dialog>"

data Name
  = UrlBar
  | ContentViewport
  | TextPrompt
  | ContentList
  deriving (Show, Eq, Ord)

type Event = PageResponse

makeLenses ''State

app :: App State Event Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = Focus.focusRingCursor (^. focusRing),
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const myAttrMap
    }

maxPrefetchLinks = 30

myAttrMap :: AttrMap
myAttrMap =
  applyAttrMappings Banner.attrs $
    Brick.attrMap
      V.defAttr
      [ (BrickList.listSelectedAttr, V.black `on` V.white),
        ("geminiH1", V.currentAttr `withStyle` bold `withStyle` underline),
        ("geminiH2", V.currentAttr `withStyle` underline),
        ("geminiH3", V.currentAttr `withStyle` bold),
        ("geminiUriUnknown", fg blue),
        ("geminiUriLoading", fg yellow),
        ("geminiUriLoaded", fg green),
        ("geminiUriErrored", fg red),
        ("yellow", fg yellow),
        ("geminiPre", fg blue),
        (Dialog.buttonAttr, fg white),
        (Dialog.buttonSelectedAttr, black `on` yellow)
      ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent e) = handleVtyEvent s e
handleEvent s (AppEvent response) = handleIncomingGeminiResponse s response

handleIncomingGeminiResponse :: State -> PageResponse -> EventM Name (Next State)
handleIncomingGeminiResponse s response =
  case response of
    PageResponse uri response historyBehavior -> do
      let page = Page.loaded uri response
      -- s' <- liftIO $ whenMaybe (historyBehavior /= History.Cache) (prefetch response s)
      -- continue $ responseToState (fromMaybe s s') historyBehavior response
      continue $ navigateToPage page historyBehavior s
    NoPageResponse uri -> do
      -- todo
      -- continue $ s & cache %~ Cache.insert uri NetErr
      continue s

prefetch :: Page -> State -> IO State
prefetch page s =
  let uris = Page.links page
   in if length uris <= maxPrefetchLinks
        then preemptivelyLoad uris s
        else return s

preemptivelyLoad :: [URI] -> State -> IO State
preemptivelyLoad uris s =
  let urisToLoad = filter (\u -> Cache.missing u (s ^. cache)) uris
   in foldM (\s uri -> liftIO (startLoading uri History.Cache s)) s urisToLoad

pageFromCache :: URI -> State -> Loadable GeminiResponse
pageFromCache uri s =
  case s ^. cache & Cache.get uri of
    Just page -> page ^. Page.content
    Nothing -> NotStarted

currentTab :: State -> Tab
currentTab s =
  fromMaybe Tab.new (s ^. tabs & CList.focus)

setCurrentTab :: State -> Tab -> State
setCurrentTab s tab =
  s & tabs .~ CList.update tab (s ^. tabs)

currentTabL :: Lens' State Tab
currentTabL =
  lens currentTab setCurrentTab

currentPage :: State -> Page
currentPage s =
  currentTab s ^. Tab.page

isLoading :: State -> Bool
isLoading s =
  Page.isLoading $ currentPage s

cycleFocus s =
  continue $ s & focusRing %~ Focus.focusNext

handleVtyEvent :: State -> V.Event -> EventM Name (Next State)
handleVtyEvent s e =
  case (getFocus s, e) of
    (Just UrlBar, V.EvKey V.KEnter []) ->
      let editContents =
            s ^. urlBar & Editor.getEditContents & head
       in if isLoading s
            then continue s
            else case URI.parseURI editContents of
              Just uri -> do
                liftIO (doFetch uri History.Push s) >>= continue
              Nothing ->
                continue s
    (Just UrlBar, V.EvKey (V.KChar '\t') []) ->
      cycleFocus s
    (Just ContentViewport, V.EvKey (V.KChar '\t') []) ->
      cycleFocus s
    (_, V.EvKey (V.KChar 'c') [V.MCtrl]) ->
      halt s
    (Just UrlBar, V.EvKey (V.KChar 'f') [V.MCtrl]) ->
      continue $ s & urlBar %~ Editor.applyEdit Zipper.moveRight
    (Just UrlBar, V.EvKey (V.KChar 'b') [V.MCtrl]) ->
      continue $ s & urlBar %~ Editor.applyEdit Zipper.moveLeft
    (Just UrlBar, ev) ->
      continue
        =<< handleEventLensed s urlBar handleEditorEvent e
    -- todo
    -- (Just TextPrompt, V.EvKey V.KEnter []) -> do
    --   case (s ^. currentURI, Dialog.dialogSelection (s ^. promptDialog)) of
    --     (Just currentUri, Just (Just _)) ->
    --       let query =
    --             s ^. textPrompt
    --               & Editor.getEditContents
    --               & unlines
    --               & stripString
    --           newUri = setQuery currentUri query
    --        in liftIO (doFetch newUri History.Push s) >>= continue
    --     (Just currentUri, Just Nothing) ->
    --       continue $ s & currentTabL %~ Tab.back
    --     (Just currentUri, Nothing) ->
    --       continue s
    --     (Nothing, _) ->
    --       error "wut."
    (Just TextPrompt, ev) -> do
      s' <- handleEventLensed s promptDialog Dialog.handleDialogEvent e
      s'' <- handleEventLensed s' textPrompt handleEditorEvent e
      continue s''
    (Just ContentViewport, V.EvKey V.KEnter []) -> do
      let selection = s ^. contentList & BrickList.listSelectedElement
      case selection of
        Just (n, LinkLine (Right uri) desc) ->
          liftIO (doFetch uri History.Push s) >>= continue
        _ ->
          continue s
    (Just ContentViewport, V.EvKey V.KBS []) ->
      continue $ s & currentTabL %~ Tab.back
    (Just ContentViewport, ev) ->
      continue
        =<< handleEventLensed s contentList handleListEvent e
    _ ->
      continue s

saneEscapeUri =
  URI.escapeURIString URI.isAllowedInURI

setQuery :: URI -> String -> URI
setQuery uri query =
  uri {URI.uriQuery = "?" <> saneEscapeUri query}

handleEditorEvent ::
  (DecodeUtf8 t, Eq t, Monoid t) =>
  V.Event ->
  Editor t n ->
  EventM n (Editor t n)
handleEditorEvent ev ed =
  -- decorate with some emacsy keybindings
  case ev of
    V.EvKey (V.KChar 'f') [V.MCtrl] ->
      return $ Editor.applyEdit Zipper.moveRight ed
    V.EvKey (V.KChar 'b') [V.MCtrl] ->
      return $ Editor.applyEdit Zipper.moveLeft ed
    _ ->
      Editor.handleEditorEvent ev ed

textPromptActive :: State -> Bool
textPromptActive s = Page.isLoaded (currentPage s)

handleListEvent =
  BrickList.handleListEventVi BrickList.handleListEvent

doFetch :: URI -> History.Behavior -> State -> IO State
doFetch uri historyBehavior s =
  case Cache.get uri (s ^. cache) of
    Just page -> do
      -- todo: prefetch cachedResponse s
      -- todo: wait for page to load before initting contentList
      return $ navigateToPage page historyBehavior s
    -- return $ responseToState s pushHistory cachedResponse
    -- Just Loading ->
    --   return s
    _ ->
      startLoading uri historyBehavior s

navigateToPage :: Page -> History.Behavior -> State -> State
navigateToPage page historyBehavior s =
  -- todo: handle Input, Errors, Other content types
  -- todo: handle page not yet loaded
  s & currentTabL %~ Tab.show page historyBehavior
    & urlBar .~ makeUrlEditor (URI.uriToString id (page ^. Page.uri) "")
    & contentList %~ BrickList.listReplace (Vector.fromList $ Page.lines page) (Just 0)
    & focusRing .~ (Focus.focusSetCurrent ContentViewport focusRingNormal)
    & cache %~ Cache.set page

startLoading :: URI -> History.Behavior -> State -> IO State
startLoading uri pushHistory s = do
  let request = PageRequest uri pushHistory
  -- STM.atomically $ STM.writeTChan (s ^. killChan) KillRequest
  -- STM.atomically $ STM.readTChan (s ^. killChan) -- sync
  STM.atomically $ STM.writeTChan (s ^. requestChan) request
  return $ s & cache %~ Cache.setLoading uri

makeUrlEditor :: String -> Editor String Name
makeUrlEditor =
  Editor.editor UrlBar (Just 1)

-- responseToState :: State -> History.Behavior -> GeminiResponse -> State
-- responseToState s historyBehavior responseData =
--   let pushHistory v =
--         case (historyBehavior, s ^. currentURI) of
--           (History.Push, Just oldUri) -> Vector.snoc v oldUri
--           _ -> v
--    in if historyBehavior == Cache
--         then s & cache %~ Map.insert uri (Loaded geminiResponse)
--         else case responseData of
--           Input promptText ->
--             s & textPrompt .~ Editor.editor TextPrompt (Just 1) ""
--               & cache %~ Map.insert uri (Loaded geminiResponse)
--               & currentURI ?~ uri
--               & focusRing .~ (Focus.focusSetCurrent TextPrompt focusRingPrompt)
--               & history %~ pushHistory
--               & promptDialog
--                 .~ Dialog.dialog
--                   (Just (T.unpack promptText))
--                   (Just (1, [("Back", Nothing), ("Submit", Just promptText)]))
--                   32
--           Success content ->
--             let lines =
--                   case content of
--                     GeminiResource pageLines ->
--                       pageLines
--                     UnknownResource mimeType text ->
--                       fmap TextLine (T.lines text)
--              in s & currentURI ?~ uri
--                   & urlBar .~ makeUrlEditor (URI.uriToString id uri "")
--                   & contentList %~ BrickList.listReplace (Vector.fromList lines) (Just 0)
--                   & focusRing .~ (Focus.focusSetCurrent ContentViewport focusRingNormal)
--                   & history %~ pushHistory
--                   & cache %~ Map.insert uri (Loaded geminiResponse)
--           other ->
--             -- todo: fixme
--             let bogusMessage =
--                   [ TextLine "welp...",
--                     TextLine (T.pack $ show other)
--                   ]
--              in s & currentURI ?~ uri
--                   & urlBar .~ makeUrlEditor (URI.uriToString id uri "")
--                   & contentList %~ BrickList.listReplace (Vector.fromList bogusMessage) (Just 0)
--                   & focusRing .~ (preservingFocus s focusRingNormal)
--                   & history %~ pushHistory
--                   & cache %~ Map.insert uri (Loaded geminiResponse)

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
  [ center $
      border $
        vBox
          [ drawUrlBar s,
            hBorder,
            drawMainContent s,
            hBorder,
            drawStatusLine s
          ]
  ]

drawUrlBar :: State -> Widget Name
drawUrlBar s =
  Editor.renderEditor
    (str . unlines)
    (hasFocus s UrlBar)
    (s ^. urlBar)

drawMainContent s =
  vBox $
    [currentPage s & drawPage s]
      ++ [fill ' ' | isLoading s]

drawStart =
  let drawLabel =
        str "press " <+> (yellowStr "<return>") <+> str " to start"
   in center $ borderWithLabel drawLabel $ Banner.draw

yellowStr =
  withAttr "yellow" . str

drawPage :: State -> Page -> Widget Name
drawPage s page =
  drawLoadable
    (page ^. Page.content)
    ( \case
        Success _ ->
          BrickList.renderList
            (\highlighted line -> (displayLine s line) <+> str " ")
            (hasFocus s ContentViewport)
            (s ^. contentList)
        Input inputStr ->
          vBox
            [ Dialog.renderDialog
                (s ^. promptDialog)
                (drawInputEdit s),
              fill ' '
            ]
        resp ->
          str "Unknown response!" <=> (str $ show resp)
    )

drawLoadable :: Loadable a -> (a -> Widget Name) -> Widget Name
drawLoadable loadable draw =
  case loadable of
    NotStarted -> drawStart
    Loading -> str "Loading..."
    Loaded a -> draw a
    NetErr -> str "Error!"

drawInputEdit s =
  border $
    Editor.renderEditor
      (str . unlines)
      (hasFocus s TextPrompt)
      (s ^. textPrompt)

displayLine :: State -> Line -> Widget Name
displayLine s line =
  case mapLine cleanupLine line of
    HeadingLine H1 t -> withAttr "geminiH1" $ txt t
    HeadingLine H2 t -> withAttr "geminiH2" $ txt t
    HeadingLine H3 t -> withAttr "geminiH3" $ txt t
    TextLine t -> withAttr "geminiText" $ txtWrap t
    ULLine t -> withAttr "geminiUl" $ txt ("• " <> t)
    PreLine t -> withAttr "geminiPre" $ txt t
    QuoteLine t -> withAttr "geminiQuote" $ txtWrap ("> " <> t)
    LinkLine uri desc -> displayLink s uri desc

cleanupLine :: Text -> Text
cleanupLine t =
  -- tabs and CRs screw up Brick's text wrapping
  T.replace "\t" "  " t & T.filter Char.isPrint & T.stripEnd

mapLine :: (Text -> Text) -> Line -> Line
mapLine fn = \case
  HeadingLine h t -> HeadingLine h (fn t)
  TextLine t -> TextLine (fn t)
  ULLine t -> ULLine (fn t)
  PreLine t -> PreLine (fn t)
  QuoteLine t -> QuoteLine (fn t)
  LinkLine uri desc -> LinkLine uri (fn desc)

displayLink :: State -> Either Text URI -> Text -> Widget Name
displayLink s uriOrErr desc =
  let uriBit =
        case uriOrErr of
          Right uri ->
            str "(" <+> (showUri uri) <+> str ") " <+> (drawStatus uri)
          Left foo ->
            txt ("<URI parse failed!> - " <> foo)

      uriToTxt uri =
        T.pack $ URI.uriToString id uri ""

      drawStatus uri =
        let status =
              case (s ^. cache & Cache.get uri) of
                Just page ->
                  case page ^. Page.content of
                    Loaded content ->
                      if isGemErr content then "geminiUriErrored" else "geminiUriLoaded"
                    Loading -> "geminiUriLoading"
                    _ -> "geminiUriUnknown"
                _ -> "geminiUriUnknown"
         in (withAttr status $ str "▲")

      showUri uri = hyperlink (uriToTxt uri) $ withAttr "yellow" $ txt (uriToTxt uri)
   in txt desc <+> str " " <+> uriBit

drawStatusLine :: State -> Widget Name
drawStatusLine s =
  let histCount =
        currentTab s ^. Tab.history & History.count

      drawHistory =
        case currentTab s ^. Tab.history & History.pop of
          (_, Just lastPage) ->
            str $
              "("
                <> (show $ histCount)
                <> ") Last page: "
                <> show lastPage
                <> " (backspace)"
          _ ->
            str "No history"
      drawMime =
        str $ (getMimeType s) <> " "

      drawCacheSize =
        let total = show $ Cache.count (s ^. cache)
            loaded =
              show $
                Cache.countWhere Page.isLoaded (s ^. cache)
         in str $ total <> "/" <> loaded <> " "

      help =
        str "(Ctrl+c to quit)"
   in padRight Max drawHistory <+> drawCacheSize <+> help

getMimeType :: State -> String
getMimeType s = case currentPage s & Page.contentMay of
  Just (Success (UnknownResource mimeType txt)) ->
    T.unpack mimeType
  _ -> "?"

isGemErr :: GeminiResponse -> Bool
isGemErr (Failure _) = True
isGemErr _ = False

initState :: TChan PageRequest -> TChan KillRequest -> String -> State
initState requests kill initUri =
  State
    { _tabs = CList.singleton Tab.new,
      _cache = Cache.new,
      _requestChan = requests,
      _killChan = kill,
      _urlBar = makeUrlEditor initUri,
      _focusRing = focusRingNormal,
      _contentList = BrickList.list ContentList Vector.empty 1,
      _promptDialog = Dialog.dialog Nothing Nothing 99,
      _textPrompt = Editor.editor TextPrompt (Just 1) ""
    }

runApp :: String -> IO ()
runApp initUri = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  (requests, kill, responses) <- RequestProcessor.start
  let state = initState requests kill initUri
  void $ customMain initialVty buildVty (Just responses) app state

main :: IO ()
main = runApp "gemini://gemini.circumlunar.space/"
