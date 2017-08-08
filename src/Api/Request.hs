{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Api.Request where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as C8

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
    setRequestMethod (C8.pack "GET") $ addRequestPath arguments httpRequest
build (PostRequest arguments) httpRequest = httpRequest
build (PutRequest arguments) httpRequest = httpRequest
build (DeleteRequest arguments) httpRequest = httpRequest
build (PatchRequest arguments) httpRequest = httpRequest


addRequestPath :: [String] -> Request -> Request
addRequestPath [] request = request
addRequestPath xs request = setRequestPath (C8.pack $ "/2.1" ++ head xs) request
