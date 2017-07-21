{-# LANGUAGE DeriveGeneric #-}
module Config where

import GHC.Generics
import Data.Yaml
import System.Posix.Files

data Config = Config
                {
                    id :: String,
                    secret :: String
                } deriving (Show, Generic)

instance FromJSON Config

confirmExistence :: Maybe FilePath -> IO Bool
confirmExistence Nothing = return False 
confirmExistence (Just file) = fileExist file

load :: Maybe FilePath -> IO (Maybe Config)
load Nothing = return Nothing
load (Just file) =
    do
        settings <- decodeFile(file)
        return $ settings
