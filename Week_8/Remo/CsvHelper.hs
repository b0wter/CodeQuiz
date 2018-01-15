{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module CsvHelper where

import Data.Csv
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as L8

-- Encode and decode functions for CSV with header

justRecords (Left    msg  ) = Nothing
justRecords (Right (h, rs)) = Just rs 

decodeStrWith f = f . decodePreprocess
decodeFileWith f fp = decodeStrWith f <$> L8.readFile fp

-- Helper functions

decodePreprocess = lowerFirstLine . dropBom

lowerFirstLine s = let (h, rows) = L8.span (/= '\n') s in L8.concat [L8.map toLower h, rows]

dropBom s = case L8.stripPrefix bom s of
             Nothing -> s
             Just r  -> r

bom :: L8.ByteString
bom = "\239\187\191"

