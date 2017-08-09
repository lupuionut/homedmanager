{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Api.Request where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types.Header
import System.FilePath.Posix

data ApiRequest a =
      PostRequest a
    | GetRequest a
    | PutRequest a
    | DeleteRequest a
    | PatchRequest a
    deriving (Show, Generic)

data ReceivedError = ReceivedError { msg::String, code::String }
    deriving (Show, Read, Generic)

instance FromJSON ReceivedError


build :: ApiRequest [String]
        -> [(C8.ByteString, Maybe C8.ByteString)]
        -> Request
        -> Request
build (GetRequest arguments) options httpRequest =
    setRequestMethod (C8.pack "GET")
    $ getRequest arguments httpRequest
build (PostRequest arguments) options httpRequest =
    setRequestMethod (C8.pack "POST")
    $ postRequest arguments options httpRequest
build (PutRequest arguments) options httpRequest =
    setRequestMethod (C8.pack "PUT") httpRequest
build (DeleteRequest arguments) options httpRequest =
    setRequestMethod (C8.pack "DELETE") httpRequest
build (PatchRequest arguments) options httpRequest =
    setRequestMethod (C8.pack "PATCH") httpRequest


getRequest :: [String] -> Request -> Request
getRequest [] request = request
getRequest xs request = setRequestPath (C8.pack $ "/2.1" ++ head xs) request


postRequest :: [String]
    -> [(C8.ByteString, Maybe C8.ByteString)]
    -> Request
    -> Request
postRequest [] options request = request
postRequest (path:arguments) options request =
    case path of
        "/file" -> postFile arguments options req
        _ -> req
    where
        req = setRequestPath (C8.pack $ "/2.1" ++ path) request


-- | https://api.hidrive.strato.com/2.1/static/apidoc/index.html#/2.1/file_POST
postFile :: [String]
    -> [(C8.ByteString, Maybe C8.ByteString)]
    -> Request
    -> Request
postFile [] options request = request
postFile arguments options request =
    setRequestBodyFile file $
    addRequestHeader hContentType (C8.pack "application/octet-stream") $
    setRequestQueryString (options ++ [("name",fileName file)]) $
    request
    where
        file = head arguments

fileName :: String -> Maybe C8.ByteString
fileName "" = Nothing
fileName s = pure $ C8.pack $ takeFileName s



