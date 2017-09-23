{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( runBot
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Data.Maybe
import Data.IORef
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot

import CommonTypes
import Config
import Receiver
import Sender


runBot :: FilePath -> IO ()
runBot configFileName = do
    config <- loadConfig configFileName
    initialBotState <- initBotState
    eventLoop config initialBotState

initBotState :: IO BotState
initBotState = do
    manager <- newManager tlsManagerSettings
    initialUpdateId <- newIORef Nothing
    let initialBotState = BotState { _httpManager = manager
                                   , _mostRecentUpdateId = initialUpdateId }
    return initialBotState

eventLoop :: Config -> BotState -> IO ()
eventLoop config botState = do
    runMaybeT $ pulse config botState
    threadDelay 100000
    eventLoop config botState

pulse :: Config -> BotState -> MaybeT IO ()
pulse config botState = do
    msgs <- getNewMessages config botState
    forM_ msgs $ processMessage config botState

processMessage :: Config -> BotState -> MessageFromUser -> MaybeT IO ()
processMessage config botState messageFromUser = do
    liftIO $ print messageFromUser
    liftIO $ writeIORef (_mostRecentUpdateId botState) (Just $ _updateId messageFromUser)
