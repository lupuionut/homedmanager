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


data AppRequest
type instance HidriveResponse AppRequest = AppInfoResponse

data AppInfoResponse = AppInfoResponse
    {
        appCreated :: Int,
        appDeveloper :: Developer,
        appHomepage :: String,
        appId :: Int,
        appMayPublish :: Bool,
        appName :: String,
        appPrivateFolder :: String,
        appPrivateFolderId :: String,
        appPublicationUrl :: String,
        appRefreshToken :: RefreshToken,
        appStatus :: String
    } deriving Show

data Developer = Developer
    {
        devEmail :: String,
        devName :: String
    } deriving Show

data RefreshToken = RefreshToken
    {
        refreshTokenExpire :: Int,
        refreshTokenExpireIn :: Int,
        refreshTokenInstanceId :: String,
        refreshTokenScope :: String
    } deriving Show

{--
instance FromJSON AppInfoResponse where
    parseJSON = withObject "AppInfoResponse" parse
        where
            parse o = AppInfoResponse 
                        <$> o .: "created"
                        <*> o .: "developer.email"
                        <*> o .: "developer.name"
                        <*> o .: "homepage"
                        <*> o .: "id"
                        <*> o .: "may_publish"
   --}                     




data PermissionsRequest
type instance HidriveResponse PermissionsRequest = PermissionsResponse

data PermissionsResponse = PermissionsResponse
  { 
    permissionsWritable :: Bool,
    permissionsReadable :: Bool
  } deriving (Eq, Show)

instance FromJSON PermissionsResponse where
  parseJSON = withObject "PermissionsResponse" parse
    where
      parse o = PermissionsResponse
                <$> o .: "writable"
                <*> o .: "readable"
