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

data ResponseTokens = ResponseTokens { refresh_token :: String, expires_in :: Int, userid :: String, access_token :: String, alias :: String, token_type :: String } deriving (Generic, Read, Show)
data ResponseTokenInfo = ResponseTokenInfo { expires_in :: Int, client_id :: String, alias :: String, userid :: String, scope :: String } deriving (Generic, Read, Show)
data ResponseError = ResponseError { error::String, error_description :: String } deriving (Generic, Read, Show)

instance FromJSON ResponseTokens
instance FromJSON ResponseTokenInfo
instance FromJSON ResponseError

-- | helper function for getting new tokens/new token info
doRequest :: URL -> [(C8.ByteString, Maybe C8.ByteString)] -> IO (Either String ResponseTokens)
doRequest url queries =
    do
        initial <- H.parseRequest url
        let req = H.setRequestMethod "POST"
                $ H.setRequestQueryString queries
                $ H.setRequestSecure True
                $ H.setRequestPort 443
                $ initial
        res <- H.httpJSON req
        case (H.getResponseStatusCode res) of
            200 -> return $ Right ((H.getResponseBody res))
            401 -> fail "You must reauthorize this app. After authorization, edit homedmanager.yaml with the new provided code"
            _ -> fail ("Request to auth server failed." ++ show (H.getResponseStatusCode res))


-- | retrieve access_token and refresh_token using code provided in authorize step (from config file)
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#token
grantWithCode :: Maybe Config -> IO (Maybe ResponseTokens)
grantWithCode Nothing = return Nothing
grantWithCode (Just c) =
    do
        response <- doRequest (Config.authTokenUrl c) [("grant_type", Just "authorization_code"),("code", Just (C8.pack $ Config.authCode c)),("client_id", Just (C8.pack $ Config.client_id c)),("client_secret", Just (C8.pack $ Config.client_secret c))]
        case response of
            Left _ -> return Nothing
            Right r -> return $ pure r


-- | retrieve access_token using stored refresh token from the tokens.cache file
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#token
grantWithRefreshToken :: Maybe Config -> IO (Maybe ResponseTokens)
grantWithRefreshToken Nothing = return Nothing
grantWithRefreshToken (Just c) =
    do
        response <- doRequest (Config.authTokenUrl c) [("grant_type", Just "refresh_token"),("client_id", Just (C8.pack $ Config.client_id c)),("client_secret", Just (C8.pack $ Config.client_secret c))]
        case response of
            Left _ -> return Nothing
            Right r -> return $ pure r


-- | verify access_token
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#tokeninfo
verifyToken :: String -> URL -> IO (Either ResponseError ResponseTokenInfo)
verifyToken "" _ = return $ Left ResponseError { error = "1", error_description = "Invalid url to verify token"}
verifyToken _ "" = return $ Left ResponseError { error = "1", error_description = "Invalid url to verify token"}
verifyToken t url =
    do
        initial <- H.parseRequest (url)
        let req = H.setRequestMethod "POST"
                $ H.setRequestQueryString [("access_token", Just (C8.pack t))]
                $ H.setRequestSecure True
                $ H.setRequestPort 443
                $ initial
        res <- H.httpLBS req
        let body = H.getResponseBody res
        case (H.getResponseStatusCode res) of
            200 -> do
                    let bd = decode body :: Maybe ResponseTokenInfo
                    return $ Right (fromJust bd)
            _ -> do
                    let bd = decode body :: Maybe ResponseError
                    return $ Left (fromJust bd)


-- | provide reqtokens to use in the API calls
withAccessToken :: Maybe Config -> IO (Maybe ResponseTokens)
withAccessToken Nothing = return Nothing
withAccessToken cfg = do
                        loaded <- loadTokens
                        case loaded of
                            Nothing -> do
                                (grantWithCode cfg) >>= storeTokens; loadTokens
                            (Just tokens) -> do
                                verif <- verifyToken (access_token tokens) (authTokenInfo (fromJust cfg))
                                case verif of
                                    Left _ -> return Nothing
                                    Right info ->
                                        if (expires_in (info::ResponseTokenInfo) > 0)
                                            then return loaded
                                            else do (grantWithRefreshToken cfg) >>= storeTokens; loadTokens


-- | store response from server to tokens.cache
storeTokens :: Maybe ResponseTokens -> IO ()
storeTokens Nothing = return ()
storeTokens (Just t) = Fs.mkOrRetTokenCacheFile >>= (\f -> Fs.storeInFile f (show t))


-- | loads content of tokens.cache
loadTokens :: IO (Maybe ResponseTokens)
loadTokens =
    Fs.mkOrRetTokenCacheFile >>= Fs.loadFromFile >>=
    (\s -> case (length s) of
                0 -> return Nothing
                _ -> return $ Just (read s))
