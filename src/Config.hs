{-# LANGUAGE DeriveGeneric #-}
module Config where

import GHC.Generics
import Data.Yaml

data Config = Config
                {
                    id :: String,
                    secret :: String
                } deriving (Show, Generic)

instance FromJSON Config


load :: Maybe FilePath -> IO (Maybe Config)
load Nothing = return Nothing
load (Just file) =
    do
        settings <- decodeFile(file)
        return $ settings
