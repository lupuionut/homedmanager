{-# LANGUAGE OverloadedStrings #-}
module Api where

import qualified Network.HTTP.Simple as H
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as C8
import qualified Codec.Binary.Base64.String as B64
import Api.Request
import Data.Char
import Data.Aeson
import Data.Maybe
import System.Environment

type Token = String
type Command = [String]
type Options = Maybe [(String,String)]


execute :: String-> Command -> Token -> Options -> IO ()
execute u c t o = case request of
    Nothing -> return ()
    Just action -> do
        let options = buildInternalOptions o
        let token = "Bearer " ++ B64.encode t
        response <- doRequest u token action options
        putStrLn $ response
    where
        request = buildInternalRequest c


buildInternalRequest :: Command -> Maybe (ApiRequest Command)
buildInternalRequest [] = Nothing
buildInternalRequest (x:xs) =
    case (toLowerS x) of
        "get" ->
            Just (GetRequest xs)
        "post" ->
            Just (PostRequest xs)
        "put" ->
            Just (PutRequest xs)
        "delete" ->
            Just (DeleteRequest xs)
        "patch" ->
            Just (PatchRequest xs)
        _ -> Nothing


buildInternalOptions :: Options -> [(C8.ByteString, Maybe C8.ByteString)]
buildInternalOptions Nothing = []
buildInternalOptions (Just o) =
    map (\x -> (C8.pack $ fst x, pure $ C8.pack $ snd x)) o


doRequest :: String
    -> Token
    -> ApiRequest Command
    -> [(C8.ByteString, Maybe C8.ByteString)]
    -> IO String
doRequest url token action options =
    do
        initial <- H.parseRequest url
        let composed = Api.Request.build action options initial
        let req = H.addRequestHeader hAuthorization (C8.pack token)
                -- $ H.setRequestQueryString options
                $ H.setRequestSecure True
                $ H.setRequestPort 443
                $ composed
        return $ show req
        res <- H.httpLBS req
        let body = H.getResponseBody res
        case (H.getResponseStatusCode res) of
            200 -> do
                return $ show $ fromJust (decode body :: Maybe Value)
            201 -> do
                return $ show $ fromJust (decode body :: Maybe Value)
            _ -> do
                return $ msg $ fromJust (decode body :: Maybe ReceivedError)


toLowerS :: String -> String
toLowerS = map toLower
