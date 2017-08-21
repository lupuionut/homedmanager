{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types where
import GHC.Generics
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Simple

data ReceivedError = ReceivedError { msg::String, code::String }
    deriving (Show, Read, Generic)
instance FromJSON ReceivedError

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
    } deriving (Eq,Show)

data Developer = Developer
    {
        devEmail :: Maybe String,
        devName :: Maybe String
    } deriving (Show,Eq)

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
    } deriving (Eq,Show)

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
  } deriving (Eq, Show)

instance FromJSON PermissionsResponse where
  parseJSON = withObject "PermissionsResponse" parse
    where
      parse o = PermissionsResponse
                <$> o .:? "account"
                <*> o .:  "writable"
                <*> o .:  "readable"
                <*> o .:? "path"
{-- /permission requests --}
