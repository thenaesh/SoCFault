{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config
    ( loadConfig
    , Config (..)
    , DBConfig (..)
    ) where

import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Text
import Data.Aeson
import GHC.Generics


loadConfig :: FilePath -> IO Config
loadConfig fileName = do
    file <- B.readFile path
    case decode file of
        Nothing -> error errorMsg -- uncoverable error, terminate the program
        Just config -> return config
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
