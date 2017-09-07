module Print (  printSharelink,
                printUploadResponse,
                printShareResponse,
                printPermissionsResponse,
                printListDirResponse,
                printSharelinkrmResponse ) where

import Api.Types
import Data.Maybe
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as BUTF

printSharelink :: Either String (HidriveResponse ShareLinkRequest)
    -> IO ()
printSharelink response = case response of
    Right response -> do
        mapM_ putStrLn $ map formatSharelink response
    Left error -> print error


formatSharelink :: ShareLinkObject -> String
formatSharelink o =
    "Id: " ++
    (if isJust(shareLinkObjectId o)
        then show . fromJust $ shareLinkObjectId o
        else "Unknown") ++ "\n" ++
    "Path: " ++
    (if isJust(shareLinkObjectPath o)
        then show . fromJust $ shareLinkObjectPath o
        else "Unknown") ++ "\n" ++
    "Status: " ++
    (if isJust(shareLinkObjectStatus o)
        then show . fromJust $ shareLinkObjectStatus o
        else "Unknown") ++ "\n" ++
    "Number of downloads: " ++
    (if isJust(shareLinkObjectCount o)
        then show . fromJust $ shareLinkObjectCount o
        else "Unknown") ++ "\n" ++
    "Created: " ++
    (if isJust(shareLinkObjectCreated o)
        then show . fromJust $ shareLinkObjectCreated o
        else "Unknown") ++ "\n" ++
    "Has passw: " ++
    (if isJust(shareLinkObjectHasPassword o)
        then show . fromJust $ shareLinkObjectHasPassword o
        else "Unknown") ++ "\n" ++
    "Last modif: " ++
    (if isJust(shareLinkObjectLastModified o)
        then show . fromJust $ shareLinkObjectLastModified o
        else "Unknown") ++ "\n" ++
    "Max count: " ++
    (if isJust(shareLinkObjectMaxcount o)
        then show . fromJust $ shareLinkObjectMaxcount o
        else "Unknown") ++ "\n" ++
    "Name: " ++
    (if isJust(shareLinkObjectName o)
        then show . fromJust $ shareLinkObjectName o
        else "Unknown") ++ "\n" ++
    "Uri: " ++
    (if isJust(shareLinkObjectUri o)
        then show . fromJust $ shareLinkObjectUri o
        else "Unknown") ++ "\n" ++
    "Writable: " ++
    (if isJust(shareLinkObjectWritable o)
        then show . fromJust $ shareLinkObjectWritable o
        else "Unknown") ++ "\n\n"


printUploadResponse :: Either String (HidriveResponse UploadFileRequest)
    -> IO ()
printUploadResponse response = case response of
    Right r -> do
        putStrLn . formatUploadResponse $ r
    Left e -> print e


formatUploadResponse :: UploadFileResponse -> String
formatUploadResponse o =
    "Name: " ++ (encodeUtfUrlStr $ uploadFileName o) ++ "\n" ++
    "Creation time: " ++ (show $ uploadFileCtime o) ++ "\n" ++
    "Path: " ++ (encodeUtfUrlStr $ uploadFilePath o) ++ "\n" ++
    "Size: " ++ (show $ uploadFileSize o) ++ "\n" ++
    "Writable: " ++ (show $ uploadFileWritable o)


printShareResponse :: Either String (HidriveResponse ShareRequest)
    -> IO ()
printShareResponse response = case response of
    Right r -> do
        putStrLn . formatShareResponse $ r
    Left e -> print e


formatShareResponse :: ShareResponse -> String
formatShareResponse o =
    "URI: " ++ shareUri o ++ "\n" ++
    "Downloads allowed: " ++ (show $ shareMaxCount o) ++ "\n" ++
    "Path: " ++ (encodeUtfUrlStr $ sharePath o)


printPermissionsResponse ::
    Either String (HidriveResponse PermissionsRequest)
    -> IO ()
printPermissionsResponse response = case response of
    Right r -> do
        putStrLn . formatPermissionsResponse $ r
    Left e -> print e


formatPermissionsResponse :: PermissionsResponse -> String
formatPermissionsResponse o =
    "Id: " ++
    (if isJust (permissionsAccount o)
        then fromJust $ permissionsAccount o
        else "Unknown") ++ "\n"
    ++ "Writable: " ++ (show $ permissionsWritable o) ++ "\n"
    ++ "Readable: " ++ (show $ permissionsReadable o) ++ "\n"
    ++ "Path: " ++
    (if isJust (permissionsPath o)
        then show . fromJust $ permissionsPath o
        else "Unknown")

printListDirResponse ::
    Either String (HidriveResponse ListDirRequest)
    -> IO ()
printListDirResponse response = case response of
    Right r -> do
        putStrLn . formatListDirResponse 0 $ r
    Left e -> print e


formatListDirResponse :: Int -> ListDirResponse -> String
formatListDirResponse n o =
    "\n - " ++
    rep ++ "Name: " ++
    (if isJust(listdirName o)
        then encodeUtfUrlStr $ fromJust $ listdirName o
        else "Unknown") ++ "\n"
    ++ rep ++ "Path: " ++
    (if isJust(listdirPath o)
        then encodeUtfUrlStr $ fromJust $ listdirPath o
        else "Unknown") ++ "\n"
    ++ rep ++ "Type: " ++
    (if isJust(listdirType o)
        then fromJust $ listdirType o
        else "Unknown") ++ "\n"
    ++ rep ++ "Members: " ++
    (if isJust(listdirMembers o)
        then unwords $ fmap (formatListDirResponse (n+1)) $ fromJust $ listdirMembers o
        else "Unknown") ++ "\n"
    where
        rep = unwords $ replicate n "\t"


printSharelinkrmResponse ::
    Either String (HidriveResponse Void)
    -> IO ()
printSharelinkrmResponse response = case response of
    Right r -> do
        return ()
    Left e -> print e


encodeUtfUrlStr :: String -> String
encodeUtfUrlStr s =
    BUTF.toString $
    urlDecode True $
    BUTF.fromString $
    s
