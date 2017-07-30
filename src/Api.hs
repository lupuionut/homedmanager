{-# LANGUAGE OverloadedStrings #-}
module Api where

import qualified Network.HTTP.Simple as H
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as C8
import qualified Codec.Binary.Base64.String as B64
import Api.Request
import Data.Char
import System.Environment

type Token = String
type Command = [String]
type Options = Maybe [(String,String)]


execute :: String-> Command -> Token -> Options -> IO ()
execute u c t o = case request of
    Nothing -> return ()
    Just action -> do
        let options = buildInternalOptions o
        let url = u ++ endpoint action
        let token = "Bearer " ++ B64.encode t
        response <- doRequest url token action options
        putStrLn $ response
    where
        request = buildInternalRequest c


buildInternalRequest :: Command -> Maybe (Action String)
buildInternalRequest [] = Nothing
buildInternalRequest (x:xs)
    | toLowerS x == toLowerS "GET" = Just (GET (unwords xs))
    | toLowerS x == toLowerS "POST" = Just (POST (unwords xs))
    | toLowerS x == toLowerS "PUT" = Just (PUT (unwords xs))
    | toLowerS x == toLowerS "DELETE" = Just (DELETE (unwords xs))
    | toLowerS x == toLowerS "PATCH" = Just (PATCH (unwords xs))
    | otherwise = Just (POST (unwords xs))


buildInternalOptions :: Options -> [(C8.ByteString, Maybe C8.ByteString)]
buildInternalOptions Nothing = []
buildInternalOptions (Just o) =
    map (\x -> (C8.pack $ fst x, pure $ C8.pack $ snd x)) o


doRequest :: String
    -> Token
    -> Action String
    -> [(C8.ByteString, Maybe C8.ByteString)]
    -> IO String
doRequest url token action options =
    do
        initial <- H.parseRequest url
        let req = H.setRequestMethod (C8.pack $ method action)
                $ H.addRequestHeader hAuthorization (C8.pack token)
                $ H.setRequestQueryString options
                $ H.setRequestSecure True
                $ H.setRequestPort 443
                $ initial
        res <- H.httpLBS req
        let body = H.getResponseBody res
        case (H.getResponseStatusCode res) of
            200 -> do
                return $ show body
            _ -> do
                return $ show (options)


toLowerS :: String -> String
toLowerS = map toLower
