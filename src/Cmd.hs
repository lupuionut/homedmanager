module Cmd (extract, toExecute, find) where

import System.Environment
import Control.Monad

-- | extract specific arguments from the command line
-- | e.g. --config
extract :: String -> IO (Maybe String)
extract cmd = do
    input <- getArgs
    return $ find cmd input

find :: String -> [String] -> Maybe String
find _ [] = Nothing
find cmd str = if length found < 2 then Nothing else Just (head $ tail found)
    where
        found = dropWhile (\s -> s /= cmd) str

-- | parse the command line and gets the command to execute
-- | by convention, any string starting with -- is considered to be an argument
-- | e.g. --config
toExecute :: IO (Maybe [String])
toExecute = do
    input <- getArgs
    return $ parse input

parse :: [String] -> Maybe [String]
parse [] = Nothing
parse (s:[]) = if (head s == '-') then Just[""] else Just [s]
parse (s:ss)
    | (head s == '-') = 
        if length (tail ss) == 0 then Just[""] else parse (tail ss)
    | otherwise = liftM2 (++) (Just [s]) (parse ss)
