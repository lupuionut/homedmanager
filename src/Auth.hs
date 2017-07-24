{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Auth where

import GHC.Generics
import qualified Network.HTTP.Simple as H
import qualified Data.ByteString.Char8 as C8
import Config
import Data.Maybe
import Data.Aeson

data ReqTokens = ReqTokens  {
                        refresh_token :: String,
                        expires_in :: Int,
                        userid :: String,
                        access_token :: String,
                        alias :: String,
                        token_type :: String,
                        scope :: String
                    } deriving (Generic, Show)


instance FromJSON ReqTokens

requestAccessToken :: Maybe Config -> IO ()
requestAccessToken Nothing = putStrLn ""
requestAccessToken (Just c) = do
    r <- H.parseRequest (Config.authTokenUrl c)
    let req = H.setRequestMethod "POST"
            $ H.setRequestQueryString [("hello", Just "world")]
            $ H.setRequestSecure True
            $ H.setRequestPort 443
            $ r
    res <- H.httpJSON req
    let response = H.getResponseBody res :: ReqTokens
    print response
