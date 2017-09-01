{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Api where

import Network.HTTP.Types.Header
import qualified Network.HTTP.Simple as H
import qualified Data.ByteString.Char8 as C8
import qualified Codec.Binary.Base64.String as B64
import qualified Data.ByteString.UTF8 as BUTF
import System.FilePath.Posix
import Data.Aeson
import Data.Maybe
import System.Environment
import Api.Types
import Print

type Token = String
type Options = Maybe [(String,String)]


execute :: String-> [String] -> Token -> Options -> IO ()
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
        "permissions" -> do
            let req = permissions options request'
            response <- execute' req
            print response
        "ls" -> do
            let req = lsDir options request'
            response <- execute' req
            print response
        "upload" -> do
            let fpath = extractUploadPath c
            case fpath of
                Nothing -> putStrLn "Please provide file path of file to upload"
                Just f -> do
                    let req = upload options request' f
                    response <- execute' req
                    printUploadResponse response
        "unlink" -> do
            let req = unlink options request'
            response <- execute' req
            print response
        "share" -> do
            let req = share options request'
            response <- execute' req
            print response
        "sharelink" -> do
            let req = sharelink options request'
            response <- execute' req
            printSharelink response 
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
        204 -> do
            return $ Left "No body response"
        _ -> do
            return $ Left $ msg $ fromJust $
                (decode body :: Maybe ReceivedError)


buildInternalOptions :: Options -> [(C8.ByteString, Maybe C8.ByteString)]
buildInternalOptions Nothing = []
buildInternalOptions (Just o) =
    map (\x -> (C8.pack $ fst x, pure $ C8.pack $ snd x)) o


extractUploadPath :: [String] -> Maybe FilePath
extractUploadPath [] = Nothing
extractUploadPath xs = case (length xs) of
    1 -> Nothing
    _ -> pure . head . tail $ xs


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
    -> HidriveRequest ListDirRequest H.Request
lsDir options httpReq = request
    where
        request = mkHidriveRequest
                    (C8.pack "GET")
                    (H.setRequestMethod (C8.pack "GET") $
                    H.setRequestPath (C8.pack("/2.1/dir")) $
                    H.setRequestQueryString options
                    httpReq)


upload :: [(C8.ByteString, Maybe C8.ByteString)]
    -> H.Request
    -> FilePath
    -> HidriveRequest UploadFileRequest H.Request
upload options httpReq file = request
    where
        request = mkHidriveRequest
                    (C8.pack "POST")
                    (H.setRequestMethod (C8.pack "POST") $
                    H.setRequestPath (C8.pack("/2.1/file")) $
                    H.addRequestHeader
                        hContentType
                        (C8.pack ("application/octet-stream")) $
                    H.setRequestBodyFile file $
                    H.setRequestQueryString 
                        (options ++ [("name",fileName file)]) $
                    httpReq)

fileName :: String -> Maybe C8.ByteString
fileName "" = Nothing
fileName s = pure $ BUTF.fromString $ takeFileName s


unlink :: [(C8.ByteString, Maybe C8.ByteString)]
    -> H.Request
    -> HidriveRequest Void H.Request
unlink options httpReq = request
    where
        request = mkHidriveRequest
                (C8.pack "DELETE")
                (H.setRequestMethod (C8.pack "DELETE") $
                H.setRequestPath (C8.pack("/2.1/file")) $
                H.setRequestQueryString options
                httpReq)


share :: [(C8.ByteString, Maybe C8.ByteString)]
    -> H.Request
    -> HidriveRequest ShareRequest H.Request
share options httpReq = request
    where
        options' = options ++ [(C8.pack("type"), Just (C8.pack $ "file"))]
        request = mkHidriveRequest
                (C8.pack "POST")
                (H.setRequestMethod (C8.pack "POST") $
                H.setRequestPath (C8.pack("/2.1/sharelink")) $
                H.setRequestQueryString options'
                httpReq)


sharelink :: [(C8.ByteString, Maybe C8.ByteString)]
    -> H.Request
    -> HidriveRequest ShareLinkRequest H.Request
sharelink options httpReq = request
    where
        request = mkHidriveRequest
                (C8.pack "GET")
                (H.setRequestMethod (C8.pack "GET") $
                H.setRequestPath (C8.pack("/2.1/sharelink")) $
                H.setRequestQueryString options
                httpReq)
