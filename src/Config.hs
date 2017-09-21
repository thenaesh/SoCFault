{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config
    ( loadConfig
    , Config (..)
    , DBConfig (..)
    ) where

import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text
import Data.Aeson
import GHC.Generics


loadConfig :: FilePath -> MaybeT IO Config
loadConfig fileName = MaybeT $ do
    file <- B.readFile path
    let config = decode file
    when (isNothing config) $ putStrLn errorMsg
    return config
    where
        path = mconcat [basePath, "/", fileName]
        basePath = "config"
        errorMsg = "Error reading " ++ fileName ++ "!"

data Config = Config
    { _botToken :: !Text
    , _dbConfig :: !DBConfig
    } deriving (Show, Generic)

instance FromJSON Config

data DBConfig = DBConfig
    { _host :: !Text
    , _db :: !Text
    , _user :: !Text
    , _password :: !Text
    } deriving (Show, Generic)

instance FromJSON DBConfig
