{-# LANGUAGE DeriveGeneric #-}

module Response where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Iowa

instance ToJSON ResponseHeader

data ResponseHeader = RespHdr { requestTime :: UTCTime,
                                resultCount :: Int
                              } deriving (Generic, Show)

mkResponse :: UTCTime -> [a] -> (ResponseHeader, [a])
mkResponse tm rd = (RespHdr tm (length rd), rd)

