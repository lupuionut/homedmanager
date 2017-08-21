module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.Exit
import Data.Maybe
import Config
import Auth
import Api
import Cmd

main :: IO ()
main = do
    cmdline <- getArgs
    homedir <- getHomeDirectory

    let command = Cmd.buildCommand cmdline
    let options = Cmd.buildOptions $ Cmd.parseOptions cmdline
    let cfgOption = Cmd.extractOptionValue "config" options
    let cfgFile = cfgOption <|> (Just (homedir ++ "/homedmanager.yaml"))
    if (unwords command == "help") then
        putStrLn help
    else do
        cfg <-
            Config.confirmExistence cfgFile >>=
                (\res -> if res then Config.load cfgFile else return Nothing)
        Auth.withAccessToken cfg >>=
            (\mt -> case mt of
                Nothing -> putStrLn $ "Could not get a valid access_token"
                Just respToken -> do
                    let withToken = Auth.access_token respToken
                    let url = Config.apiUrl (fromJust cfg)
                    Api.execute url command withToken options
            )


help :: String
help =
    "\nUsage: homedmanager-exe COMMAND [OPTIONS] \n" ++
    "Each option (parameter) is the format -key=values." ++
    "For example -fields=account,alias\n" ++
    "You can use multiple options with some commands\n\n" ++
    "List of commands:\n" ++
    "\t help\t\t\t display this help\n" ++
    "\t app\t\t\t display app related information\n" ++
    "\t stat -path=\t\t call to get access information for file/folder." 
    ++ " You must provide a value for the path parameter"
