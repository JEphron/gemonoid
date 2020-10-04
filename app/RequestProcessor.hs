module RequestProcessor (start, PageRequest (..), KillRequest (..), PageResponse (..)) where

import qualified Brick.BChan as BChan
import Client (GeminiResponse)
import qualified Client
import Control.Concurrent (ThreadId, forkIO, killThread)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever)
import qualified History
import Network.URI (URI)

data PageRequest
  = PageRequest URI History.Behavior

data PageResponse
  = PageResponse GeminiResponse History.Behavior
  | NoPageResponse URI

data KillRequest = KillRequest | KillResponse

start :: IO (STM.TChan PageRequest, STM.TChan KillRequest, BChan.BChan PageResponse)
start = do
  requests <- STM.atomically STM.newTChan
  kill <- STM.atomically STM.newTChan
  responses <- BChan.newBChan 10
  multithread (process requests responses) 10
  return (requests, kill, responses)

multithread :: IO () -> Int -> IO [ThreadId]
multithread threadBuilder nThreads =
  sequence $ replicate nThreads $ forkIO threadBuilder

process :: STM.TChan PageRequest -> BChan.BChan PageResponse -> IO ()
process requests responses =
  forever $ do
    PageRequest uri hist <- STM.atomically $ STM.readTChan requests
    response <- Client.get uri
    case response of
      Just r ->
        BChan.writeBChan responses (PageResponse r hist)
      Nothing ->
        BChan.writeBChan responses (NoPageResponse uri)

--supervisor :: IO () -> Int -> STM.TChan KillRequest -> IO ()
--supervisor threadBuilder nThreads kill =
--  forever $ do
--    tids <- multithread threadBuilder nThreads
--    STM.atomically $ STM.readTChan kill
--    mapM_ killThread tids
--    STM.atomically $ STM.writeTChan kill KillResponse
