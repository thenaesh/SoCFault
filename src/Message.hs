module Message
    ( MessageDataInterface (..)
    , MessageFromUser (..)
    , MessageToUser (..)
    ) where

import Data.Int
import Data.Text
import Web.Telegram.API.Bot

import Config


class MessageDataInterface msg where
    _chatId :: msg -> Int64
    _messageText :: msg -> Text

data MessageFromUser = MessageFromUser
    { fromUser_chatId :: Int64
    , fromUser_messageText :: Text
    , _sender :: User
    } deriving Show

instance MessageDataInterface MessageFromUser where
    _chatId = fromUser_chatId
    _messageText = fromUser_messageText

data MessageToUser = MessageToUser
    { toUser_chatId :: Int64
    , toUser_messageText :: Text
    } deriving Show

instance MessageDataInterface MessageToUser where
    _chatId = toUser_chatId
    _messageText = toUser_messageText
