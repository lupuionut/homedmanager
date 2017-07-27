{-# LANGUAGE DeriveGeneric #-}
module Config where

import GHC.Generics
import Data.Yaml
import System.Posix.Files
import Fs

data Config = Config
                {
                    client_id :: String,
                    client_secret :: String,
                    authCode :: String,
                    authTokenUrl :: String,
                    authTokenInfoUrl :: String
                } deriving (Show, Generic)

instance FromJSON Config


-- | confirm existance of configuration file
-- | this should be located in the /homedir/homedmanager.yaml
confirmExistence :: Maybe FilePath -> IO Bool
confirmExistence Nothing = return False
confirmExistence (Just file) = fileExist file


-- | loads the configuration file
-- | this is located in the /homedir/homedmanager.yaml
load :: Maybe FilePath -> IO (Maybe Config)
load Nothing = return Nothing
load (Just file) =
    do
        settings <- decodeFile(file)
        return $ settings


