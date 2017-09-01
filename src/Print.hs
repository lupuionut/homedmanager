module Print (  printSharelink,
                printUploadResponse ) where

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
    "Name: " ++ (
        BUTF.toString $
        urlDecode True $
        BUTF.fromString $
        uploadFileName o) ++ "\n" ++
    "Creation time: " ++ (show $ uploadFileCtime o) ++ "\n" ++
    "Path: " ++ (
        BUTF.toString $
        urlDecode True $
        BUTF.fromString $
        uploadFilePath o) ++ "\n" ++
    "Size: " ++ (show $ uploadFileSize o) ++ "\n" ++
    "Writable: " ++ (show $ uploadFileWritable o)
