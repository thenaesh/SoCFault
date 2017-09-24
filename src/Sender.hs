{-# LANGUAGE OverloadedStrings #-}

module Sender
    ( sendMessagesToChat
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Int
import Data.Maybe
import Data.Text hiding (filter)
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot

import CommonTypes (BotState (..), MessageToUser (..))
import Config


createSendMessageRequest :: Int64 -> Text -> SendMessageRequest
createSendMessageRequest chatId msg = defaultSendMessageRequest { message_chat_id = ChatId chatId , message_text = msg }
    where
        defaultSendMessageRequest = SendMessageRequest
            { message_parse_mode = Nothing
            , message_disable_web_page_preview = Nothing
            , message_disable_notification = Nothing
            , message_reply_to_message_id = Nothing
            , message_reply_markup = Nothing
            , message_chat_id = ChatId (-1) -- default value to avoid warning, hopefully it's not a meaningful chat ID
            , message_text = "" --default value to avoid warning
            }

sendMessagesToChat :: Config -> BotState -> [MessageToUser] -> MaybeT IO ()
sendMessagesToChat config botState msgs = forM_ msgs $ sendMessageToChat config botState

sendMessageToChat :: Config -> BotState -> MessageToUser -> MaybeT IO ()
sendMessageToChat config botState msg = MaybeT $ do
    let req = createSendMessageRequest (_chatId msg) (_messageText msg)
    let token = Token $ _botToken config
    let manager = _httpManager botState
    res <- sendMessage token req manager
    case res of
        Left e -> do
            return Nothing
        Right u -> do
            return $ Just ()
