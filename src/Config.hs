module Config (fromCmdLine)
where

import System.IO
import System.Environment

fromCmdLine :: IO (Maybe String)
fromCmdLine = do
    args <- getArgs
    return $ parse args


parse :: [String] -> Maybe String
parse ("--config":ss) = Just (unwords ss)
parse [] = Nothing
parse _ = Nothing
