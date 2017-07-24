module Main where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Exit
import Data.Maybe
import Cmd
import Config
import Auth

main :: IO ()
main = do
    cfgFile <- liftM2 (\cli sys -> cli <|> sys)
                (Cmd.extract "--config")
                (getHomeDirectory >>= (\path -> return $ Just (path ++ "/homedmanager.yaml")))
    cfg <- Config.confirmExistence cfgFile >>= (\res -> if res then Config.load cfgFile else return Nothing)
    -- putStrLn $ (show cfg)
    c <- Config.storageDir
    print c
