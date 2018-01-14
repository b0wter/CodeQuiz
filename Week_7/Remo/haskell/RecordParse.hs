module RecordParse (tryParse) where

import Records
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

tryParse :: MsgRecord -> RecordName -> L8.ByteString -> Either MsgRecord Record
tryParse deflt recordName s = case decodeR recordName s of
                                Nothing -> case decodeR Message s of 
                                           Nothing -> Left deflt
                                           Just (M msg) -> Left msg
                                Just rec -> Right rec

decodeR :: RecordName -> L8.ByteString -> Maybe Record
decodeR Foo s     = (decode s :: Maybe FooRecord) >>= (Just . F)
decodeR AEntity s = (decode s :: Maybe AEntityRecord) >>= (Just . A)
decodeR BEntity s = (decode s :: Maybe BEntityRecord) >>= (Just . B)
decodeR CEntity s = (decode s :: Maybe CEntityRecord) >>= (Just . C)
decodeR Listing s = (decode s :: Maybe [ListRecord]) >>= (Just . L)
decodeR Message s = (decode s :: Maybe MsgRecord) >>= (Just . M)

