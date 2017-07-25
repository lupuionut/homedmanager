{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Auth where

import GHC.Generics
import qualified Network.HTTP.Simple as H
import qualified Data.ByteString.Char8 as C8
import Fs
import Config
import Data.Maybe
import Data.Aeson

type URL = String
data ReqTokens = ReqTokens  {
                        refresh_token :: String,
                        expires_in :: Int,
                        userid :: String,
                        access_token :: String,
                        alias :: String,
                        token_type :: String,
                        scope :: String
                    } deriving (Generic, Read, Show)

data TokenInfo = TokenInfo {
                        expires_in :: Int,
                        client_id :: Int,
                        alias :: String,
                        scope :: String
                    } deriving (Generic, Read, Show)


instance FromJSON ReqTokens
instance FromJSON TokenInfo


-- | helper function for getting new tokens/new token info
doRequest :: URL -> [(C8.ByteString, Maybe C8.ByteString)] -> IO (Either String ReqTokens)
doRequest url queries =
    do
        initial <- H.parseRequest url
        let req = H.setRequestMethod "POST"
                $ H.setRequestQueryString queries
                $ H.setRequestSecure True
                $ H.setRequestPort 443
                $ initial
        res <- H.httpJSON req
        case show(H.getResponseStatus res) of
            "200" -> return $ Right ((H.getResponseBody res) :: ReqTokens)
            "401" -> fail "You must reauthorize this app. After authorization, edit homedmanager.yaml with the new provided code"
            _ -> fail "Request to auth server failed."


-- | retrieve access_token and refresh_token using code provided in authorize step (from config file)
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#token
grantWithCode :: Maybe Config -> IO (Maybe ReqTokens)
grantWithCode Nothing = return Nothing
grantWithCode (Just c) =
    do
        response <- doRequest (Config.authTokenUrl c) [("grant_type", Just "authorization_code"),("code", Just (C8.pack $ Config.authCode c)),("client_id", Just "id"),("client_secret", Just "secret")]
        case response of
            Left _ -> return Nothing
            Right r -> return $ pure r


-- | retrieve access_token using stored refresh token from the tokens.cache file
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#token
grantWithRefreshToken :: Maybe Config -> IO (Maybe ReqTokens)
grantWithRefreshToken Nothing = return Nothing
grantWithRefreshToken (Just c) =
    do
        response <- doRequest (Config.authTokenUrl c) [("grant_type", Just "refresh_token"),("client_id", Just "id"),("client_secret", Just "secret")]
        case response of
            Left _ -> return Nothing
            Right r -> return $ pure r


-- | verify access_token
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#tokeninfo
verifyToken :: String -> URL -> IO (Either String TokenInfo)
verifyToken "" _ = return $ Left "Invalid token to verify."
verifyToken _ "" = return $ Left "Invalid url to verify token."
verifyToken t url =
    do
        initial <- H.parseRequest (url)
        let req = H.setRequestMethod "POST"
                $ H.setRequestQueryString [("access_token", Just (C8.pack t))]
                $ H.setRequestSecure True
                $ H.setRequestPort 443
                $ initial
        res <- H.httpJSON req
        case show(H.getResponseStatus res) of
            "200" -> return $ Right ((H.getResponseBody res) :: TokenInfo)
            _ -> return $ Left "Invalid token"



-- |
withAccessToken :: Maybe Config -> IO ()
withAccessToken = undefined


-- | store response from server to tokens.cache
storeTokens :: Maybe ReqTokens -> IO ()
storeTokens Nothing = return ()
storeTokens (Just t) = Config.mkOrRetStorageDir >>= (\storage -> Fs.storeInFile (storage ++ "/tokens.cache") (show t))


-- | loads content of tokens.cache
loadTokens :: IO (Maybe ReqTokens)
loadTokens =
    Config.mkOrRetStorageDir >>=
    (\storage -> Fs.loadFromFile (storage ++ "/tokens.cache")) >>=
    (\s -> case (length s) of
                0 -> return Nothing
                _ -> return $ Just (read s))
