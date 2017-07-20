module Main where

import Data.Maybe
import Cmd

main :: IO ()
main = do
    extracted <- Cmd.toExecute
    let command = case extracted of
                        Just a -> a
                        otherwise -> ["nothing"]
    putStrLn $ unwords command
