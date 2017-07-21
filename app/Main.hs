module Main where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Posix.Files
import System.Exit
import Data.Maybe
import Cmd
import Config

main :: IO ()
main = do
    cfgFile <- liftM2 (\cli sys -> cli <|> sys)
                (Cmd.extract "--config")
                (getHomeDirectory >>= (\path -> return $ Just (path ++ "/homedmanager.yaml")))
    cfg <- Config.load cfgFile 
    putStrLn $ (show cfg)
