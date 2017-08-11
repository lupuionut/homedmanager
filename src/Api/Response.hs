{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Response where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Api.Request
import Data.ByteString
import Control.Applicative


-- | handle different responses and return the value to output
showResponse :: Response -> String
showResponse response =
    case response of
        GetDir a -> do
            unwords $ Prelude.map (\x -> x ++ "\n") $ Prelude.map (show . name) a
        GetAppMe a -> a
        Dyn b -> show b


type Members = [Member]
data Member = Member { name :: String } deriving (Show, Read, Generic)
instance FromJSON Member


data Response =
    GetDir {members :: Members}
    | GetAppMe {getAppMe :: String}
    | Dyn Value
    deriving (Show)

instance FromJSON Response where
  parseJSON v =
    getDirParse v <|>
    getAppMeParse v <|>
    defParse v


getDirParse :: Value -> Parser Response
getDirParse = withObject "obj" (\o -> GetDir <$> o .: "members")

getAppMeParse :: Value -> Parser Response
getAppMeParse = withObject "obj" (\o -> GetAppMe <$> o .: "name")

defParse :: Value -> Parser Response
defParse v = pure (Dyn v)
