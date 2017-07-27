{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Auth (access_token, withAccessToken) where

import GHC.Generics
import qualified Network.HTTP.Simple as H
import qualified Data.ByteString.Char8 as C8
import Fs
import Config
import Data.Maybe
import Data.Aeson

type URL = String

data ResponseTokens = ResponseTokens {
        refresh_token :: String,
        expires_in :: Int,
        userid :: String,
        access_token :: String,
        alias :: String,
        token_type :: String } deriving (Generic, Read, Show)

data ResponseTokenInfo = ResponseTokenInfo {
        expires_in :: Int,
        client_id :: String,
        alias :: String,
        userid :: String,
        scope :: String } deriving (Generic, Read, Show)

data ResponseError = ResponseError {
        error::String,
        error_description :: String } deriving (Generic, Read, Show)

instance FromJSON ResponseTokens
instance FromJSON ResponseTokenInfo
instance FromJSON ResponseError

-- | helper function for getting new tokens/new token info

doRequest :: URL
    -> Config
    -> [(C8.ByteString, Maybe C8.ByteString)]
    -> IO (Either String ResponseTokens)
doRequest url cfg queries =
    do
        initial <- H.parseRequest url
        let req = H.setRequestMethod "POST"
                $ H.setRequestQueryString queries
                $ H.setRequestSecure True
                $ H.setRequestPort 443
                $ initial
        res <- H.httpLBS req
        let body = H.getResponseBody res
        case (H.getResponseStatusCode res) of
            200 -> do
                let bd = decode body :: Maybe ResponseTokens
                return $ Right (fromJust bd)
            401 -> do
                let bd = decode body :: Maybe ResponseError
                let reauthMessage = reauthorizeAppMessage cfg
                fail $ (error_description (fromJust bd)) ++ reauthMessage
            _ -> do
                let bd = decode body :: Maybe ResponseError
                return $ Left (error_description $ fromJust bd)


-- | retrieve access_token and refresh_token using code provided in authorize
--   step (from config file)
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#token
grantWithCode :: Maybe Config -> IO (Maybe ResponseTokens)
grantWithCode Nothing = return Nothing
grantWithCode (Just c) =
    do
        response <-
            doRequest (Config.authTokenUrl c) c
                        [ ("grant_type", Just "authorization_code"),
                        ("code", Just (C8.pack $ Config.authCode c)),
                        ("client_id", Just (C8.pack $ Config.client_id c)),
                        ("client_secret", Just (C8.pack $ Config.client_secret c))]
        case response of
            Left err -> fail err
            Right r -> return $ pure r


-- | retrieve access_token using stored refresh token from the tokens.cache file
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#token
grantWithRefreshToken :: Maybe Config -> String -> IO (Maybe ResponseTokens)
grantWithRefreshToken Nothing refreshToken = return Nothing
grantWithRefreshToken (Just c) refreshToken =
    do
        response <-
            doRequest (Config.authTokenUrl c) c
                        [ ("grant_type", Just "refresh_token"),
                        ("refresh_token", Just(C8.pack refreshToken)),
                        ("client_id", Just (C8.pack $ Config.client_id c)),
                        ("client_secret", Just (C8.pack $ Config.client_secret c))]
        case response of
            Left err -> fail err
            Right r -> return $ pure r


-- | verify access_token
--   https://dev.strato.com/hidrive/content.php?r=150--OAuth2-Authentication#tokeninfo
verifyToken :: String
                -> Config
                -> URL
                -> IO (Either ResponseError ResponseTokenInfo)
verifyToken "" _ _ = return $ Left ResponseError
    {error = "1", error_description = "Empty token."}
verifyToken _ cfg "" = return $ Left ResponseError
    {error = "1", error_description = "Empty token url."}
verifyToken t cfg url =
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
            401 -> do
                    let bd = decode body :: Maybe ResponseError
                    let reauthMessage = reauthorizeAppMessage cfg
                    fail $ (error_description (fromJust bd)) ++ reauthMessage
            _ -> do
                    let bd = decode body :: Maybe ResponseError
                    return $ Left (fromJust bd)


-- | provide reqtokens to use in the API calls
withAccessToken :: Maybe Config -> IO (Maybe ResponseTokens)
withAccessToken Nothing = return Nothing
withAccessToken cfg =
    do
        loaded <- loadTokens
        case loaded of
            Nothing -> do
                (grantWithCode cfg) >>= storeTokens; loadTokens
            (Just tokens) -> do
                let config = fromJust cfg
                let refreshToken = refresh_token tokens
                tokenInfo <- verifyToken
                                (access_token tokens)
                                config
                                (authTokenInfoUrl config)
                case tokenInfo of
                    Left err -> do
                        (grantWithRefreshToken cfg refreshToken) >>=
                            storeTokens
                        loadTokens
                    Right info ->
                        if (expires_in (info::ResponseTokenInfo) > 0)
                            then return loaded
                            else do
                                (grantWithRefreshToken cfg refreshToken) >>=
                                    storeTokens
                                loadTokens


-- | store response from server to tokens.cache
storeTokens :: Maybe ResponseTokens -> IO ()
storeTokens Nothing = return ()
storeTokens (Just t) =
    Fs.mkOrRetTokenCacheFile >>= (\f -> Fs.storeInFile f (show t))


-- | loads content of tokens.cache
loadTokens :: IO (Maybe ResponseTokens)
loadTokens =
    Fs.mkOrRetTokenCacheFile >>= Fs.loadFromFile >>=
    (\s -> case (length s) of
                0 -> return Nothing
                _ -> return $ Just (read s::ResponseTokens))


reauthorizeAppMessage :: Config -> String
reauthorizeAppMessage cfg =
        "You must reauthorize this app." ++
        "Please follow this link: " ++
        "https://www.hidrive.strato.com/oauth2/authorize?" ++
        "client_id="++(Config.client_id cfg)++"&response_type=code" ++
        " Provided code must be inserted in homedmanager.yaml"
