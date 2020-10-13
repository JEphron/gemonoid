{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Browser where

import qualified Banner
import Brick
import qualified Brick.Focus as Focus
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.Dialog as Dialog
import Brick.Widgets.Edit (DecodeUtf8, Editor)
import qualified Brick.Widgets.Edit as Editor
import qualified Brick.Widgets.List as BrickList
import Cache (Cache)
import qualified Cache
import Client
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Char as Char
import Data.CircularList (CList)
import qualified Data.CircularList as CList
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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
import RequestProcessor (PageRequest, PageResponse (..))
import qualified RequestProcessor
import Tab (Tab)
import qualified Tab

data Name
  = UrlBar
  | ContentViewport
  | TextPrompt
  | ContentList
  deriving (Show, Eq, Ord)

data Browser = Browser
  { -- data
    _tabs :: CList Tab,
    _cache :: Cache,
    -- concurrency
    _requestChan :: TChan PageRequest,
    -- ui
    _focusRing :: Focus.FocusRing Name,
    _urlBar :: Editor String Name,
    _pageView :: BrickList.List Name Line,
    _promptDialog :: Dialog.Dialog (Maybe Text),
    _promptEditor :: Editor String Name
  }

makeLenses ''Browser

currentTab :: Lens' Browser Tab
currentTab =
  let getter browser =
        fromMaybe Tab.new (browser ^. tabs & CList.focus)
      setter browser tab =
        browser & tabs %~ CList.update tab
   in lens getter setter

currentPage :: Lens' Browser Page
currentPage =
  currentTab . Tab.page

isLoading :: Browser -> Bool
isLoading =
  view $ currentPage . to Page.isLoading

getFocus :: Browser -> Maybe Name
getFocus =
  view $ focusRing . to Focus.focusGetCurrent

hasFocus :: Browser -> Name -> Bool
hasFocus browser name =
  getFocus browser == Just name

cycleFocus :: Browser -> EventM Name (Next Browser)
cycleFocus s =
  continue $ s & focusRing %~ Focus.focusNext

-- INIT --

maxPrefetchLinks = 30

init :: String -> RequestProcessor.Callback -> IO Browser
init uri cb = do
  requests <- RequestProcessor.start cb
  return $
    Browser
      { _tabs = CList.singleton Tab.new,
        _cache = Cache.new,
        _urlBar = initUrlBar uri,
        _requestChan = requests,
        _focusRing = initNormalFocus,
        _pageView = BrickList.list ContentList Vector.empty 1,
        _promptDialog = Dialog.dialog Nothing Nothing 99,
        _promptEditor = Editor.editor TextPrompt (Just 1) ""
      }

-- handleIncomingGeminiResponse :: Browser -> PageResponse -> EventM Name (Next Browser)
-- handleIncomingGeminiResponse s response =
--   case response of
--     PageResponse uri response historyBehavior -> do
--       let page = Page.loaded uri response
--       -- s' <- liftIO $ whenMaybe (historyBehavior /= History.Cache) (prefetch response s)
--       -- continue $ responseToState (fromMaybe s s') historyBehavior response
--       continue $ navigateToPage page historyBehavior s
--     NoPageResponse uri -> do
--       -- todo
--       -- continue $ s & cache %~ Cache.insert uri NetErr
--       continue s

initUrlBar :: String -> Editor String Name
initUrlBar =
  Editor.editor UrlBar (Just 1)

initNormalFocus :: Focus.FocusRing Name
initNormalFocus =
  Focus.focusRing [UrlBar, ContentViewport]

initPromptFocus :: Focus.FocusRing Name
initPromptFocus =
  Focus.focusRing [UrlBar, TextPrompt]

-- EVENT --

type Event = RequestProcessor.PageResponse

handleVtyEvent :: Browser -> V.Event -> EventM Name (Next Browser)
handleVtyEvent s e =
  continue s

-- case (getFocus s, e) of
--   (Just UrlBar, V.EvKey V.KEnter []) ->
--     let editContents =
--           s ^. urlBar & Editor.getEditContents & head
--      in if isLoading s
--           then continue s
--           else case URI.parseURI editContents of
--             Just uri -> do
--               liftIO (doFetch uri History.Push s) >>= continue
--             Nothing ->
--               continue s
--   (Just UrlBar, V.EvKey (V.KChar '\t') []) ->
--     cycleFocus s
--   (Just ContentViewport, V.EvKey (V.KChar '\t') []) ->
--     cycleFocus s
--   (Just UrlBar, V.EvKey (V.KChar 'f') [V.MCtrl]) ->
--     continue $ s & urlBar %~ Editor.applyEdit Zipper.moveRight
--   (Just UrlBar, V.EvKey (V.KChar 'b') [V.MCtrl]) ->
--     continue $ s & urlBar %~ Editor.applyEdit Zipper.moveLeft
--   (Just UrlBar, ev) ->
--     continue
--       =<< handleEventLensed s urlBar handleEditorEvent e
--   -- todo
--   -- (Just TextPrompt, V.EvKey V.KEnter []) -> do
--   --   case (s ^. currentURI, Dialog.dialogSelection (s ^. promptDialog)) of
--   --     (Just currentUri, Just (Just _)) ->
--   --       let query =
--   --             s ^. promptEditor
--   --               & Editor.getEditContents
--   --               & unlines
--   --               & stripString
--   --           newUri = Utils.setQuery currentUri query
--   --        in liftIO (doFetch newUri History.Push s) >>= continue
--   --     (Just currentUri, Just Nothing) ->
--   --       continue $ s & currentTabL %~ Tab.back
--   --     (Just currentUri, Nothing) ->
--   --       continue s
--   --     (Nothing, _) ->
--   --       error "wut."
--   (Just TextPrompt, ev) -> do
--     s' <- handleEventLensed s promptDialog Dialog.handleDialogEvent e
--     s'' <- handleEventLensed s' promptEditor handleEditorEvent e
--     continue s''
--   (Just ContentViewport, V.EvKey V.KEnter []) -> do
--     let selection = s ^. pageView & BrickList.listSelectedElement
--     case selection of
--       Just (n, LinkLine (Right uri) desc) ->
--         liftIO (doFetch uri History.Push s) >>= continue
--       _ ->
--         continue s
--   (Just ContentViewport, V.EvKey V.KBS []) ->
--     continue $ s & currentTab %~ Tab.back
--   (Just ContentViewport, ev) ->
--     continue
--       =<< handleEventLensed s pageView handleListEvent e
--   _ ->
--     continue s

-- handleListEvent =
--   BrickList.handleListEventVi BrickList.handleListEvent

-- handleEditorEvent ::
--   (DecodeUtf8 t, Eq t, Monoid t) =>
--   V.Event ->
--   Editor t n ->
--   EventM n (Editor t n)
-- handleEditorEvent ev ed =
--   -- decorate with some emacsy keybindings
--   case ev of
--     V.EvKey (V.KChar 'f') [V.MCtrl] ->
--       return $ Editor.applyEdit Zipper.moveRight ed
--     V.EvKey (V.KChar 'b') [V.MCtrl] ->
--       return $ Editor.applyEdit Zipper.moveLeft ed
--     _ ->
--       Editor.handleEditorEvent ev ed

-- doFetch :: URI -> History.Behavior -> Browser -> IO Browser
-- doFetch uri historyBehavior s =
--   undefined

-- prefetch :: Page -> Browser -> IO Browser
-- prefetch page s =
--   let uris = Page.links page
--    in if length uris <= maxPrefetchLinks
--         then preemptivelyLoad uris s
--         else return s

-- preemptivelyLoad :: [URI] -> Browser -> IO Browser
-- preemptivelyLoad uris s =
--   let urisToLoad = filter (\u -> Cache.missing u (s ^. cache)) uris
--    in foldM (\s uri -> liftIO (startLoading uri History.Cache s)) s urisToLoad

-- pageFromCache :: URI -> Browser -> Loadable GeminiResponse
-- pageFromCache uri s =
--   case s ^. cache & Cache.get uri of
--     Just page -> page ^. Page.content
--     Nothing -> NotStarted

-- navigateToPage :: Page -> History.Behavior -> Browser -> Browser
-- navigateToPage page historyBehavior s =
--   -- todo: handle Input, Errors, Other content types
--   -- todo: handle page not yet loaded
--   s & currentTab %~ Tab.show page historyBehavior
--     & urlBar .~ initUrlBar (URI.uriToString id (page ^. Page.uri) "")
--     & pageView %~ BrickList.listReplace (Vector.fromList $ Page.lines page) (Just 0)
--     & focusRing .~ (Focus.focusSetCurrent ContentViewport initNormalFocus)
--     & cache %~ Cache.set page

-- startLoading :: URI -> History.Behavior -> Browser -> IO Browser
-- startLoading uri pushHistory s = do
--   let request = RequestProcessor.PageRequest uri pushHistory
--   STM.atomically $ STM.writeTChan (s ^. requestChan) request
--   return $ s & cache %~ Cache.setLoading uri

-- UI --

draw :: Browser -> [Widget Name]
draw browser =
  [ center $
      border $
        vBox
          [ --drawUrlBar browser,
            --hBorder,
            --drawMainContent browser,
            --hBorder,
            --drawStatusLine browser
            str "hello world"
          ]
  ]

-- drawMainContent :: Browser -> Widget Name
-- drawMainContent browser =
--   vBox $
--     [browser ^. currentPage & drawPage browser]
--       ++ [fill ' ' | isLoading browser]

-- drawStart :: Widget Name
-- drawStart =
--   let drawLabel =
--         str "press " <+> yellowStr "<return>" <+> str " to start"
--    in center $ borderWithLabel drawLabel $ Banner.draw

-- yellowStr =
--   withAttr "yellow" . str

-- drawPage :: Browser -> Page -> Widget Name
-- drawPage browser page =
--   drawLoadable
--     (page ^. Page.content)
--     ( \case
--         Success _ ->
--           BrickList.renderList
--             (\highlighted line -> (displayLine browser line) <+> str " ")
--             (hasFocus browser ContentViewport)
--             (browser ^. pageView)
--         Input inputStr ->
--           vBox
--             [ Dialog.renderDialog
--                 (browser ^. promptDialog)
--                 (drawInputEdit browser),
--               fill ' '
--             ]
--         resp ->
--           str "Unknown response!" <=> (str $ show resp)
--     )

-- drawLoadable :: Loadable a -> (a -> Widget Name) -> Widget Name
-- drawLoadable loadable draw =
--   case loadable of
--     NotStarted -> drawStart
--     Loading -> str "Loading..."
--     Loaded a -> draw a
--     NetErr -> str "Error!"

-- drawInputEdit :: Browser -> Widget Name
-- drawInputEdit browser =
--   border $ drawEditor browser TextPrompt promptEditor

-- drawUrlBar :: Browser -> Widget Name
-- drawUrlBar browser =
--   drawEditor browser UrlBar urlBar

-- drawEditor browser name getter =
--   Editor.renderEditor
--     (str . unlines)
--     (hasFocus browser name)
--     (browser ^. getter)

-- displayLine :: Browser -> Line -> Widget Name
-- displayLine browser line =
--   case Client.mapLine cleanupLine line of
--     HeadingLine H1 t -> withAttr "geminiH1" $ txt t
--     HeadingLine H2 t -> withAttr "geminiH2" $ txt t
--     HeadingLine H3 t -> withAttr "geminiH3" $ txt t
--     TextLine t -> withAttr "geminiText" $ txtWrap t
--     ULLine t -> withAttr "geminiUl" $ txt ("• " <> t)
--     PreLine t -> withAttr "geminiPre" $ txt t
--     QuoteLine t -> withAttr "geminiQuote" $ txtWrap ("> " <> t)
--     LinkLine uri desc -> displayLink browser uri desc

-- cleanupLine :: Text -> Text
-- cleanupLine t =
--   -- tabs and CRs screw up Brick's text wrapping
--   T.replace "\t" "  " t & T.filter Char.isPrint & T.stripEnd

-- displayLink :: Browser -> Either Text URI -> Text -> Widget Name
-- displayLink s uriOrErr desc =
--   let uriBit =
--         case uriOrErr of
--           Right uri ->
--             str "(" <+> (showUri uri) <+> str ") " <+> (drawStatus uri)
--           Left foo ->
--             txt ("<URI parse failed!> - " <> foo)

--       uriToTxt uri =
--         T.pack $ URI.uriToString id uri ""

--       drawStatus uri =
--         let status =
--               case (s ^. cache & Cache.get uri) of
--                 Just page ->
--                   case page ^. Page.content of
--                     Loaded content ->
--                       if isGemErr content then "geminiUriErrored" else "geminiUriLoaded"
--                     Loading -> "geminiUriLoading"
--                     _ -> "geminiUriUnknown"
--                 _ -> "geminiUriUnknown"
--          in (withAttr status $ str "▲")

--       showUri uri = hyperlink (uriToTxt uri) $ withAttr "yellow" $ txt (uriToTxt uri)
--    in txt desc <+> str " " <+> uriBit

-- isGemErr :: GeminiResponse -> Bool
-- isGemErr (Failure _) = True
-- isGemErr _ = False

drawStatusLine :: Browser -> Widget Name
drawStatusLine s =
  let histCount =
        s ^. currentTab ^. Tab.history & History.count

      drawHistory =
        case s ^. currentTab ^. Tab.history & History.pop of
          (_, Just lastPage) ->
            str $
              "("
                <> (show $ histCount)
                <> ") Last page: "
                <> show lastPage
                <> " (backspace)"
          _ ->
            str "No history"

      drawCacheSize =
        let total = show $ Cache.count (s ^. cache)
            loaded =
              show $
                Cache.countWhere Page.isLoaded (s ^. cache)
         in str $ total <> "/" <> loaded <> " "

      help =
        str "(Ctrl+c to quit)"
   in padRight Max drawHistory <+> drawCacheSize <+> help
