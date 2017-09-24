module BotState
    ( BotState (..)
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
