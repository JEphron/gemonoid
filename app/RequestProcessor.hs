module RequestProcessor (start, PageRequest (..), Callback, PageResponse (..)) where

import Brick.BChan (BChan)
import qualified Brick.BChan as BChan
import Client (GeminiResponse)
import qualified Client
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever)
import qualified History
import Network.URI (URI)

data PageRequest
  = PageRequest URI History.Behavior

data PageResponse
  = PageResponse URI GeminiResponse History.Behavior
  | NoPageResponse URI

type Callback = PageResponse -> IO ()

start :: Callback -> IO (TChan PageRequest)
start respond = do
  requests <- STM.atomically STM.newTChan
  multithread 10 (process respond requests)
  return requests

process :: Callback -> TChan PageRequest -> IO ()
process respond requests =
  forever $ do
    PageRequest uri hist <- STM.atomically $ STM.readTChan requests
    response <- Client.get uri
    case response of
      Just resp ->
        respond (PageResponse uri resp hist)
      Nothing ->
        respond (NoPageResponse uri)

multithread :: Int -> IO () -> IO [ThreadId]
multithread nThreads threadBuilder =
  sequence $ replicate nThreads $ forkIO threadBuilder
