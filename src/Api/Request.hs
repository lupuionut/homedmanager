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


build :: ApiRequest [String] -> Request -> Request
build (GetRequest arguments) httpRequest =
    setRequestMethod (C8.pack "GET") $ getRequest arguments httpRequest
build (PostRequest arguments) httpRequest =
    setRequestMethod (C8.pack "POST") $ postRequest arguments httpRequest
build (PutRequest arguments) httpRequest =
    setRequestMethod (C8.pack "PUT") httpRequest
build (DeleteRequest arguments) httpRequest =
    setRequestMethod (C8.pack "DELETE") httpRequest
build (PatchRequest arguments) httpRequest =
    setRequestMethod (C8.pack "PATCH") httpRequest


getRequest :: [String] -> Request -> Request
getRequest [] request = request
getRequest xs request = setRequestPath (C8.pack $ "/2.1" ++ head xs) request


postRequest :: [String] -> Request -> Request
postRequest [] request = request
postRequest (path:arguments) request =
    case path of
        "/file" -> postFile arguments req
        _ -> req
    where
        req = setRequestPath (C8.pack $ "/2.1" ++ path) request


-- | https://api.hidrive.strato.com/2.1/static/apidoc/index.html#/2.1/file_POST
postFile :: [String] -> Request -> Request
postFile [] request = request
postFile arguments request =
    setRequestBodyFile file $
    addRequestHeader hContentType (C8.pack "application/octet-stream") $
    request
    where
        file = head arguments
