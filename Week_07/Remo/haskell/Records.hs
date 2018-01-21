{-# LANGUAGE DeriveGeneric #-}

module Records where

import GHC.Generics
import Data.Text
import Data.Aeson
import Data.Aeson.Casing (camelCase, pascalCase, aesonPrefix)

data Record = A AEntityRecord                   -- parsed AEntityRecord
            | B BEntityRecord                   -- parsed BEntityRecord
            | C CEntityRecord                   -- parsed CEntityRecord
            | F FooRecord                       -- parsed FooRecord
            | L [ListRecord]                    -- parsed ListRecord's
            | M MsgRecord                       -- parsed MsgRecord
            | A' AEntityRecord CEntityRecord    -- AEntityRecord with its CEntityRecord
            | B' BEntityRecord CEntityRecord    -- BEntityRecord with its CEntityRecord
            | F' FooRecord [Record]             -- FooRecord with its A/B children (or possibly any other Record)
            deriving (Show, Generic)

data RecordName = Foo | AEntity | BEntity | CEntity | Listing | Message deriving (Show, Eq, Read)

instance ToJSON Record where
    toJSON (A  r) = toJSON r
    toJSON (B  r) = toJSON r
    toJSON (C  r) = toJSON r
    toJSON (F  r) = toJSON r
    toJSON (L  r) = toJSON r
    toJSON (M  r) = toJSON r
    toJSON (A' a c) = toJSON (a, c)
    toJSON (B' a c) = toJSON (a, c)
    toJSON (F' a c) = toJSON (a, c)

data FooRecord = FooRecord {
        fooId :: Int,
        fooName :: Text,
        fooIsClosed :: Bool,
        fooChildIds :: [Int],
        fooDetails :: CEntityRecord
    } deriving (Generic, Show)

instance FromJSON FooRecord where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

instance ToJSON FooRecord where
    toJSON = genericToJSON $ aesonPrefix camelCase

data AEntityRecord = AEntityRecord {
        aId :: Int,
        aName :: Text,
        aMax :: Int,
        aMin :: Int,
        aCEntityId :: Int
    } deriving (Generic, Show)

instance FromJSON AEntityRecord where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

instance ToJSON AEntityRecord where
    toJSON = genericToJSON $ aesonPrefix camelCase

data BEntityRecord = BEntityRecord {
        bId :: Int,
        bIsAwesome :: Bool,
        bIsTehSuck :: Bool,
        bCEntityId :: Int
    } deriving (Generic, Show)

instance FromJSON BEntityRecord where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

instance ToJSON BEntityRecord where
    toJSON = genericToJSON $ aesonPrefix camelCase 

data CEntityRecord = CEntityRecord {
        cId :: Int,
        cDescription :: Text,
        cHint :: Text
    } deriving (Generic, Show)

instance FromJSON CEntityRecord where
    parseJSON = genericParseJSON $ aesonPrefix camelCase

instance ToJSON CEntityRecord where 
    toJSON = genericToJSON $ aesonPrefix camelCase

data ListRecord = ListRecord {
        listId :: Int,
        listName :: Maybe Text
    } deriving (Generic, Show)

instance FromJSON ListRecord where
    parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance ToJSON ListRecord where
    toJSON = genericToJSON $ aesonPrefix pascalCase 

data MsgRecord = MsgRecord {
        msgDescription :: Text,
        msgErrorCode :: Int
    } deriving (Generic, Show)

instance FromJSON MsgRecord where
    parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance ToJSON MsgRecord where
    toJSON = genericToJSON $ aesonPrefix pascalCase
