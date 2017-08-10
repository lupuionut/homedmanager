{-# LANGUAGE OverloadedStrings #-}
module Api.PostRequest where

import Network.HTTP.Simple
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as BUTF
import System.FilePath.Posix


-- | https://api.hidrive.strato.com/2.1/static/apidoc/index.html#/2.1/file_POST
file :: [String]
    -> [(C8.ByteString, Maybe C8.ByteString)]
    -> Request
    -> Request
file [] options request = request
file arguments options request =
    setRequestBodyFile file $
    addRequestHeader hContentType (C8.pack "application/octet-stream") $
    setRequestQueryString (options ++ [("name",fileName file)]) $
    request
    where
        file = head arguments

-- | get file name from a path
fileName :: FilePath -> Maybe C8.ByteString
fileName "" = Nothing
fileName s = pure $ BUTF.fromString $ takeFileName s


-- | https://api.hidrive.strato.com/2.1/static/apidoc/index.html#/2.1/dir_POST
dir :: [String]
    -> [(C8.ByteString, Maybe C8.ByteString)]
    -> Request
    -> Request
dir arguments options request =
    setRequestQueryString options request

