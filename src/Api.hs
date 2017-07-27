module Api where

import Data.Maybe

type Token = String
type CommandList = Maybe[String]

execute :: Token -> CommandList -> IO ()
execute token clist = do
    let command = unwords . fromJust $ clist
    putStrLn command
