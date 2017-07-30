{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Api where

import qualified Network.HTTP.Simple as H
import qualified Data.ByteString.Char8 as C8
import qualified Codec.Binary.Base64.String as B64
import Api.Responses
import Data.Char
import Data.Maybe
import System.Environment

type Token = String
type Command = [String]
type Options = Maybe [(String,String)]
data Action a = GET a | POST a | PUT a | DELETE a | PATCH a
    deriving (Show,Read)

class ApiRequest a where
    method :: a -> String
    endpoint :: a -> String

instance ApiRequest (Action String) where

    method (GET _) = "GET"
    method (POST _) = "POST"
    method (PUT _) = "PUT"
    method (DELETE _) = "DELETE"
    method (PATCH _) = "PATCH"

    endpoint (GET xs) = xs
    endpoint (POST xs) = xs
    endpoint (PUT xs) = xs
    endpoint (DELETE xs) = xs
    endpoint (PATCH xs) = xs


execute :: Command -> Token -> Options -> IO ()
execute c t o = case request of
    Nothing -> return ()
    Just a -> do
        putStrLn $ show a
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


toLowerS :: String -> String
toLowerS = map toLower
