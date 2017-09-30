module Receiver
    ( getNewMessages
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.IORef
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot

import Config
import BotState
import Message


createGetUpdatesRequest :: Maybe Int -> IO GetUpdatesRequest
createGetUpdatesRequest nextUpdateId = return $ defaultGetUpdatesRequest { updates_offset = nextUpdateId }
    where
        defaultGetUpdatesRequest = GetUpdatesRequest
            { updates_offset = Nothing
            , updates_limit = Nothing
            , updates_timeout = Nothing
            , updates_allowed_updates = Just ["message"]
            }

getNewMessages :: Config -> BotState -> MaybeT IO [MessageFromUser]
getNewMessages config botState = MaybeT $ do
    updatesRequestDetails <- getNextUpdateId botState >>= createGetUpdatesRequest
    let updater = getUpdatesM updatesRequestDetails
    let token = Token $ _botToken config
    let manager = _httpManager botState
    updates <- runClient updater token manager
    case updates of
        Left e -> do
            putStrLn "Unable to get new messages!"
            return Nothing
        Right u -> do
            updateMostRecentUpdateId botState $ result u
            let msgs = fmap extractKeyInfoFromUpdate $ result u
            let msgs' = filter isJust msgs -- we completely ignore any malformed messages
            return $ sequence msgs'

getNextUpdateId :: BotState -> IO (Maybe Int)
getNextUpdateId botState = readIORef (_mostRecentUpdateId botState) >>= return . (fmap (+1))

updateMostRecentUpdateId :: BotState -> [Update] -> IO ()
updateMostRecentUpdateId botState updates = do
    currentMostRecentUpdateId <- readIORef (_mostRecentUpdateId botState)
    let newMostRecentUpdateId = foldr max currentMostRecentUpdateId $ fmap (Just . update_id) updates
    writeIORef (_mostRecentUpdateId botState) newMostRecentUpdateId

extractKeyInfoFromUpdate :: Update -> Maybe MessageFromUser
extractKeyInfoFromUpdate update = do
    msg <- message update
    sender <- from msg
    msgText <- text msg
    let chatId = chat_id $ chat msg
    return $ MessageFromUser chatId msgText sender
