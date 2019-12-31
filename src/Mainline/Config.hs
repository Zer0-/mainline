{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Mainline.Config
    ( getConfig
    , Settings (..)
    ) where

import Prelude hiding (readFile)
import Data.Maybe (fromJust)
import Data.Text (Text)
import System.Environment (getArgs)
import Data.ByteString.Lazy (readFile)
import Data.ByteString (ByteString)
import Control.Applicative (empty)
import Data.Text.Encoding (encodeUtf8)

import Data.Aeson (decode, (.:), FromJSON (..), Value (..))

import Network.KRPC.Types (Port)
import Architecture.Cmd (Loglevel (..))

data Settings = Settings
    { udpPorts :: [Port]
    , nMplex :: Int
    , bucketSize :: Int
    , msBetweenCallsToNode :: Int
    , scoreAggregateSeconds :: Int
    , sqlConnStr :: ByteString
    , minLoglvl :: Loglevel
    } deriving Show

instance FromJSON Settings where
    parseJSON (Object o) = Settings
        <$> o .: "udpPorts"
        <*> o .: "nMplex"
        <*> o .: "bucketSize"
        <*> o .: "msBetweenCallsToNode"
        <*> o .: "scoreAggregateSeconds"
        <*> do
            sqlConnText :: Text <- o .: "sqlConnStr"
            return $ encodeUtf8 sqlConnText

        <*> do
            loglvlStr :: Text <- o .: "minLoglvl"
            case loglvlStr of
                "DEBUG" -> return DEBUG
                "INFO" -> return INFO
                "WARNING" -> return WARNING
                _ -> error "Invalid loglevel"
            

    parseJSON _ = empty


getConfig :: IO (Settings)
getConfig = do
    args <- getArgs

    settingsFile <- readFile $ head args

    let settings :: Settings = fromJust $ decode settingsFile

    return settings
