module Receiver
    ( getNewMessages
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.IORef
import Data.Text hiding (filter)
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot

import Config
import BotState
import Message (MessageFromUser (..))


createGetUpdatesRequest :: IORef (Maybe Int) -> IO GetUpdatesRequest
createGetUpdatesRequest mostRecentUpdateId = do
    nextUpdateId <- readIORef mostRecentUpdateId >>= return . (fmap (+1))
    return $ defaultGetUpdatesRequest { updates_offset = nextUpdateId }
    where
        defaultGetUpdatesRequest = GetUpdatesRequest
            { updates_offset = Nothing
            , updates_limit = Nothing
            , updates_timeout = Nothing
            , updates_allowed_updates = Just ["message"]
            }

getNewMessages :: Config -> BotState -> MaybeT IO [MessageFromUser]
getNewMessages config botState = MaybeT $ do
    updatesRequestDetails <- createGetUpdatesRequest $ _mostRecentUpdateId botState
    let updater = getUpdatesM updatesRequestDetails
    let token = Token $ _botToken config
    let manager = _httpManager botState
    updates <- runClient updater token manager
    case updates of
        Left e -> do
            putStrLn "Unable to get new messages!"
            return Nothing
        Right u -> do
            let msgs = fmap extractKeyInfoFromUpdate $ result u
            let msgs' = filter isJust msgs -- we completely ignore any malformed messages
            forM_ msgs' $ \(Just msg) -> writeIORef (_mostRecentUpdateId botState) (Just $ _updateId msg)
            return $ sequence msgs'

extractKeyInfoFromUpdate :: Update -> Maybe MessageFromUser
extractKeyInfoFromUpdate update = do
    let updateId = update_id update
    msg <- message update
    sender <- from msg
    msgText <- text msg
    let chatId = chat_id $ chat msg
    return $ MessageFromUser updateId sender msgText chatId
