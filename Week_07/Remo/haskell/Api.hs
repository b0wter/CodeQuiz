{-# LANGUAGE OverloadedStrings #-} 

module Api (getAll, Records.RecordName(Foo, AEntity, BEntity)) where

import Records 
import RecordParse

import Data.Maybe
import Network.HTTP.Simple
import Data.List (intercalate)
import qualified Control.Logging as Log
import qualified Data.Text as T (isSuffixOf, pack)
import qualified Data.ByteString.Lazy.Char8 as L8

-- Retrieve all Entity records of some type without details or children
getPure :: RecordName -> IO [Record]
getPure en = getIdsOf en >>= getEntities en

-- Retrieve all Entity records of some type and all details/children
getAll :: RecordName -> IO [Record]
getAll en = getPure en >>= mapM getAndAddChildren

apiUrl = "http://cqbsapiquiz.azurewebsites.net/api/values"

retryText = "erneut versuchen."

data Identifier = List | Id Int

class Entity a where
    children :: a -> [Int]
    getChildren :: a -> IO [Maybe a]
    addChildren :: a -> [a] -> a

instance Entity Record where
    children (A r) = [aCEntityId r]
    children (B r) = [bCEntityId r]
    children (F r) = fooChildIds r
    children _ = []
    
    getChildren r@(A _) = mapM (getEntity CEntity) (children r)
    getChildren r@(B _) = mapM (getEntity CEntity) (children r)
    getChildren r@(F _) = do 
        a <- mapM (getEntity AEntity) (children r) 
        b <- mapM (getEntity BEntity) (children r)
        return (a ++ b)
    getChildren _ = return [Nothing]        

    addChildren (A r) ((C c):_) = A' r c
    addChildren (B r) ((C c):_) = B' r c
    addChildren (F r) c         = F' r c
    addChildren r _             = r

-- Handle retry message
isRetryMessage msg = retryText `T.isSuffixOf` (msgDescription msg)
mkRetryMessage = MsgRecord retryText 0

-- Use retry message as default for tryParse
tryParse' = tryParse mkRetryMessage

-- Retrieve and parse multiple Entity Records
getEntities :: RecordName -> [Int] -> IO [Record]
getEntities en ids = mapM (getEntity en) ids >>= return . catMaybes

-- Retrieve and parse specific Entity Record
getEntity :: RecordName -> Int -> IO (Maybe Record)
getEntity en x = do
    k <- getRecord en x
    case tryParse' en k of
     Left msg -> if isRetryMessage msg then getEntity en x else return Nothing
     Right rec -> return (Just rec)     

getAndAddChildren :: Record -> IO Record
getAndAddChildren r = getChildren r >>= return . addChildren r . catMaybes

-- Retrieve all available IDs of an Entity
getIdsOf :: RecordName -> IO [Int]
getIdsOf en = do
    lst <- get List en
    case tryParse' Listing lst of
     Left msg -> if isRetryMessage msg then getIdsOf en else return []
     Right (L lst) -> return (map listId lst)

-- Retrieve a specific Entity record using its ID
getRecord :: RecordName -> Int -> IO L8.ByteString
getRecord entity id = get (Id id) entity

-- Retrieve any Entity with Identifier (List or 0..)
get :: Identifier -> RecordName -> IO L8.ByteString
get id entity = Log.withStderrLogging $ do retry 1
    where retry 5 = fail $ "could not retrieve data, check your connection!"
          retry n = do
            let url = requestUrl entity id
            Log.log . text $ "retrieving " ++ url ++ " (" ++ show n ++ ")"
            resp <- httpLBS $ parseRequest_ url
            let code = getResponseStatusCode resp
                body = getResponseBody resp
            if code == 200
             then return body
             else do 
                Log.debug . text $ "got status " ++ show code ++ ", trying again"
                retry (succ n)

-- Build request URL from RecordName and Identifier
requestUrl :: RecordName -> Identifier -> String
requestUrl entity id = urlJoin $ urlParts entity id 
    where urlParts CEntity (Id n) = [apiUrl, show n, show CEntity]
          urlParts CEntity List   = error "no listing request possible for CEntity"
          urlParts entity List    = [apiUrl, show entity]
          urlParts entity (Id n)  = [apiUrl, show entity, show n]

-- Join url parts
urlJoin parts = intercalate "/" parts

-- Text formatting
text = T.pack
