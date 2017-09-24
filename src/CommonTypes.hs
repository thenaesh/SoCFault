{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module CommonTypes
    ( BotState (..)
    , MessageFromUser (..)
    , MessageToUser (..)
    ) where

import Data.Int
import Data.Text
import Data.IORef
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot

import Config


data BotState = BotState
    { _httpManager :: Manager
    , _mostRecentUpdateId :: IORef (Maybe Int)
    }

data MessageFromUser = MessageFromUser
    { _updateId :: Int
    , _sender :: User
    , _messageText :: Text
    , _chatId :: Int64
    } deriving Show

data MessageToUser = MessageToUser
    { _chatId :: Int64
    , _messageText :: Text
    }
