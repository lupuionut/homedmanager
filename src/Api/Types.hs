{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types where
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Simple
import Control.Applicative

data ReceivedError =
    ReceivedError { msg :: String, code :: Int }
    | ReceivedError' { msg :: String, rcode :: String }
    deriving (Show, Generic)


instance FromJSON ReceivedError where
    parseJSON v =
        parseCodeInt v <|>
        parseCodeStr v


parseCodeInt :: Value -> Parser ReceivedError
parseCodeInt = withObject "ReceivedError"
                (\o -> ReceivedError    <$> o .: "msg"
                                        <*> o .: "code")

parseCodeStr :: Value -> Parser ReceivedError
parseCodeStr = withObject "ReceivedError"
                (\o -> ReceivedError'    <$> o .: "msg"
                                        <*> o .: "code")

data HidriveRequest a b = HidriveRequest
    {
        method :: C8.ByteString,
        httpReq :: Request
    } deriving Show

mkHidriveRequest :: C8.ByteString
    -> Request
    -> HidriveRequest a b
mkHidriveRequest a b = HidriveRequest a b

type family HidriveResponse a :: *

{-- /app/me requests --}
data AppRequest
type instance HidriveResponse AppRequest = AppResponse

data AppResponse = AppResponse
    {
        appCreated :: Maybe Int,
        appDeveloper :: Maybe Developer,
        appHomepage :: Maybe String,
        appId :: Maybe Int,
        appMayPublish :: Maybe Bool,
        appName :: Maybe String,
        appPrivateFolder :: Maybe String,
        appPrivateFolderId :: Maybe String,
        appPublicationUrl :: Maybe String,
        appRefreshToken :: Maybe RefreshToken,
        appStatus :: Maybe String
    } deriving (Show)

data Developer = Developer
    {
        devEmail :: Maybe String,
        devName :: Maybe String
    } deriving (Show)

instance FromJSON Developer where
    parseJSON = withObject "Developer" parse
        where
            parse o = Developer
                        <$> o .:? "name"
                        <*> o .:? "email"

data RefreshToken = RefreshToken
    {
        refreshTokenExpire :: Maybe Int,
        refreshTokenExpireIn :: Maybe Int,
        refreshTokenInstanceId :: Maybe String,
        refreshTokenScope :: Maybe String
    } deriving (Show)

instance FromJSON RefreshToken where
    parseJSON = withObject "RefreshToken" parse
        where
            parse o = RefreshToken
                        <$> o .:? "expire"
                        <*> o .:? "expire_in"
                        <*> o .:? "instance_id"
                        <*> o .:? "scope"

instance FromJSON AppResponse where
    parseJSON = withObject "AppResponse" parse
        where
            parse o = AppResponse
                        <$> o .:? "created"
                        <*> o .:? "developer"
                        <*> o .:? "homepage"
                        <*> o .:? "id"
                        <*> o .:? "may_publish"
                        <*> o .:? "name"
                        <*> o .:? "private_folder"
                        <*> o .:? "private_folder_id"
                        <*> o .:? "publication_url"
                        <*> o .:? "refresh_token"
                        <*> o .:? "status"
{-- /app/me requests --}

{-- /permission requests --}
data PermissionsRequest
type instance HidriveResponse PermissionsRequest = PermissionsResponse

data PermissionsResponse = PermissionsResponse
  {
    permissionsAccount :: Maybe String,
    permissionsWritable :: Bool,
    permissionsReadable :: Bool,
    permissionsPath :: Maybe String
  } deriving (Show)

instance FromJSON PermissionsResponse where
  parseJSON = withObject "PermissionsResponse" parse
    where
      parse o = PermissionsResponse
                <$> o .:? "account"
                <*> o .:  "writable"
                <*> o .:  "readable"
                <*> o .:? "path"
{-- /permission requests --}

{-- /dir requests --}
data ListDirRequest
type instance HidriveResponse ListDirRequest = ListDirResponse

data ListDirResponse = ListDirResponse
    {
        listdirChash :: Maybe String,
        listdirCtime :: Maybe Int,
        listdirHas_dirs :: Maybe Bool,
        listdirId :: Maybe Int,
        listdirMembers :: Maybe [ListDirResponse],
        listdirMhash :: Maybe String,
        listdirMohash :: Maybe String,
        listdirMtime :: Maybe Int,
        listdirName :: Maybe String,
        listdirNhash :: Maybe String,
        listdirNmembers :: Maybe Int,
        listdirParentid :: Maybe String,
        listdirPath :: Maybe String,
        listdirReadable :: Maybe Bool,
        listdirRshare :: Maybe [LsShare],
        listdirSize :: Maybe Int,
        listdirType :: Maybe String,
        listdirWritable :: Maybe Bool
    } deriving Show

instance FromJSON ListDirResponse where
  parseJSON = withObject "ListDirResponse" parse
    where
      parse o = ListDirResponse
                <$> o .:? "chash"
                <*> o .:? "ctime"
                <*> o .:? "has_dirs"
                <*> o .:? "id"
                <*> o .:? "members"
                <*> o .:? "mhash"
                <*> o .:? "mohash"
                <*> o .:? "mtime"
                <*> o .:? "name"
                <*> o .:? "nhash"
                <*> o .:? "nmembers"
                <*> o .:? "parent_id"
                <*> o .:? "path"
                <*> o .:? "readable"
                <*> o .:? "rshare"
                <*> o .:? "size"
                <*> o .:? "type"
                <*> o .:? "writable"

data LsShare = LsShare
    {
        lsShareId :: String,
        lsShareStatus :: String,
        lsShareReadable :: Bool,
        lsShareWritable :: Bool,
        lsShareCount :: Int,
        lsSharePassword :: Maybe String,
        lsShareCreated :: Int,
        lsShareLastModified :: Int,
        lsShareShareType :: String,
        lsShareIsEncrypted :: Bool
    } deriving Show

instance FromJSON LsShare where
  parseJSON = withObject "LsShare" parse
    where
      parse o = LsShare
                    <$> o .: "id"
                    <*> o .: "status"
                    <*> o .: "readable"
                    <*> o .: "writable"
                    <*> o .: "count"
                    <*> o .:? "password"
                    <*> o .: "created"
                    <*> o .: "last_modified"
                    <*> o .: "share_type"
                    <*> o .: "is_encrypted"
{-- /dir requests --}

{-- /file POST --}
data UploadFileRequest
type instance HidriveResponse UploadFileRequest = UploadFileResponse

data UploadFileResponse = UploadFileResponse
    {
        uploadFileCtime :: Int,
        uploadFileHasDirs :: Maybe Bool,
        uploadFileId :: String,
        uploadFileImage :: FileImage,
        uploadFileMimeType :: String,
        uploadFileMtime :: Int,
        uploadFileName :: String,
        uploadFileParentId :: String,
        uploadFilePath :: String,
        uploadFileReadable :: Bool,
        uploadFileRshare :: Maybe LsShare,
        uploadFileSize :: Int,
        uploadFileType :: String,
        uploadFileWritable :: Bool
    } deriving Show

data FileImage = FileImage
    {
        fileImageExif :: Maybe FileImageExif, 
        fileImageWidth :: Maybe Int,
        fileImageHeight :: Maybe Int
    } deriving Show

data FileImageExif = FileImageExif 
    {
        resolutionUnit :: Maybe Int,
        imageHeight :: Maybe Int,
        xResolution :: Maybe Int,
        imageWidth :: Maybe Int,
        bitsPerSample :: Maybe Int,
        yResolution :: Maybe Int
    } deriving (Generic, Show)
instance FromJSON FileImageExif

instance FromJSON FileImage where
  parseJSON = withObject "FileImage" parse
    where
      parse o = FileImage 
                <$> o .:?  "exif"
                <*> o .:?  "height"
                <*> o .:?  "width"

instance FromJSON UploadFileResponse where
  parseJSON = withObject "UploadFileResponse" parse
    where
      parse o = UploadFileResponse 
                <$> o .:  "ctime"
                <*> o .:?  "has_dirs"
                <*> o .:  "id"
                <*> o .:  "image"
                <*> o .:  "mime_type"
                <*> o .:  "mtime"
                <*> o .:  "name"
                <*> o .:  "parent_id"
                <*> o .:  "path"
                <*> o .:  "readable"
                <*> o .:?  "rshare"
                <*> o .:  "size"
                <*> o .:  "type"
                <*> o .:  "writable"
{-- /file POST --}

type instance HidriveResponse Void = Void
data Void = Void deriving (Generic,Show)
instance FromJSON Void
