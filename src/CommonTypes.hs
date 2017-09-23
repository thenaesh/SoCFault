{-# LANGUAGE OverloadedStrings #-}

module CommonTypes
    ( MessageFromUser (..)
    , BotState (..)
    ) where

import Data.Text
import Data.IORef
import Network.HTTP.Client (Manager)
import Web.Telegram.API.Bot

import Config


data MessageFromUser = MessageFromUser
    { _updateId :: Int
    , _sender :: User
    , _message :: Text
    } deriving Show

data BotState = BotState
    { _httpManager :: Manager
    , _mostRecentUpdateId :: IORef (Maybe Int)
    }
