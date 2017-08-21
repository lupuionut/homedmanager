{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Api where

import Network.HTTP.Types.Header
import qualified Network.HTTP.Simple as H
import qualified Data.ByteString.Char8 as C8
import qualified Codec.Binary.Base64.String as B64
import Data.Aeson
import Data.Maybe
import System.Environment
import Api.Types


type Token = String
type Command = [String]
type Options = Maybe [(String,String)]


execute :: String-> Command -> Token -> Options -> IO ()
execute _ [] _ _ = return ()
execute endpoint c t o = do
    let options = buildInternalOptions o
    request <- H.parseRequest endpoint
    let request' = build request
    case (head c) of
        "app" -> do
            let req = infoApp request'
            response <- execute' req
            print response
        "stat" -> do
            let req = permissions options request'
            response <- execute' req
            print response
        "ls" -> do
            let req = lsDir options request'
            response <- execute' req
            print response
        _ -> putStrLn "0"
    where
        token = C8.pack ("Bearer " ++ B64.encode t)
        build request =
            H.addRequestHeader hAuthorization token $
            H.setRequestSecure True $
            H.setRequestPort 443 $
            request


execute' :: (FromJSON (HidriveResponse a))
    => HidriveRequest a H.Request
    -> IO (Either String (HidriveResponse a))
execute' req = do
    res <- H.httpLBS (httpReq req)
    let body = H.getResponseBody res
    case (H.getResponseStatusCode res) of
        200 -> do
            return $ eitherDecode body
        201 -> do
            return $ eitherDecode body
        _ -> do
            return $ Left $ msg $ fromJust
                (decode body :: Maybe ReceivedError)


buildInternalOptions :: Options -> [(C8.ByteString, Maybe C8.ByteString)]
buildInternalOptions Nothing = []
buildInternalOptions (Just o) =
    map (\x -> (C8.pack $ fst x, pure $ C8.pack $ snd x)) o


infoApp :: H.Request -> HidriveRequest AppRequest H.Request
infoApp httpReq = request
    where
        request = mkHidriveRequest
                    (C8.pack "GET")
                    (H.setRequestMethod (C8.pack "GET") $
                    H.setRequestPath (C8.pack("/2.1/app/me")) $
                    httpReq)


permissions :: [(C8.ByteString, Maybe C8.ByteString)]
    -> H.Request
    -> HidriveRequest PermissionsRequest H.Request
permissions options httpReq = request
    where
        request = mkHidriveRequest
                    (C8.pack "GET")
                    (H.setRequestMethod (C8.pack "GET") $
                    H.setRequestPath (C8.pack("/2.1/permission")) $
                    H.setRequestQueryString options
                    httpReq)

lsDir :: [(C8.ByteString, Maybe C8.ByteString)]
    -> H.Request
    -> HidriveRequest PermissionsRequest H.Request
lsDir = undefined
