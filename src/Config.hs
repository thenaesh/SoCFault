{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config
    ( loadConfig
    , Config (..)
    , DBConfig (..)
    ) where

import qualified Data.ByteString.Lazy as B
import Data.Text
import Data.Aeson
import GHC.Generics


loadConfig :: (FromJSON t) => FilePath -> IO (Maybe t)
loadConfig fileName = B.readFile path >>= (return . decode)
    where
        path = mconcat [basePath, "/", fileName]
        basePath = "config"

data Config = Config
    { botToken :: !Text
    , dbConfig :: !DBConfig
    } deriving (Show, Generic)

instance FromJSON Config

data DBConfig = DBConfig
    { host :: !Text
    , db :: !Text
    , user :: !Text
    , password :: !Text
    } deriving (Show, Generic)

instance FromJSON DBConfig
