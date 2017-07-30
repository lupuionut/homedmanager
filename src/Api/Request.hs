{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Api.Request where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as C8

data Action a = GET a | POST a | PUT a | DELETE a | PATCH a
    deriving (Show,Read)

data ReceivedError = ReceivedError { msg::String, code::String }
    deriving (Show, Read, Generic)

instance FromJSON ReceivedError

build :: Action String -> Request -> Request
build action req = case action of
    GET xs -> do
        let r = setRequestMethod "GET" req
        let path = "/2.1" ++ xs
        setRequestPath (C8.pack path) r
    POST xs -> do
        let r = setRequestMethod "POST" req
        let cmds = words xs
        r
    PUT xs -> do
        let r = setRequestMethod "PUT" req
        r
    DELETE xs -> do
        let r = setRequestMethod "DELETE" req
        r
    PATCH xs -> do
        let r = setRequestMethod "PATCH" req
        r

