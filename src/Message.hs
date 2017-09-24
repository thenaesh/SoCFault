module Message
    ( MessageFromUser (..)
    , MessageToUser (..)
    ) where

import Data.Int
import Data.Text
import Web.Telegram.API.Bot

import Config


data MessageFromUser = MessageFromUser
    { _chatId :: Int64
    , _sender :: User
    , _messageText :: Text
    } deriving Show

data MessageToUser = MessageToUser
    { _chatId :: Int64
    , _messageText :: Text
    }
