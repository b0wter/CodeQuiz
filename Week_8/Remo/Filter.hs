{-# LANGUAGE OverloadedStrings #-}

module Filter (filterRecords, limit, Accessor(..), FilterMap) where

import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Text.Lazy.Read (decimal)
import qualified Data.Text.Lazy as TL

type FilterMap a = [(Key, Accessor a)]
type Key = TL.Text
type Value = TL.Text
type KeyValue = (Key, Value)
type Filter a = a -> Bool

-- Wrapper for accessor functions
data Accessor a = AccessText     (a -> TL.Text) 
                | AccessInt      (a -> Int)
                | AccessMaybeInt (a -> Maybe Int)
                | AccessMonth    (a -> UTCTime)
                | AccessYear     (a -> UTCTime)

filterRecords :: FilterMap a -> [KeyValue] -> [a] -> [a]
filterRecords flt pm rec = foldr filter rec . mkFilters flt $ pm 

limit :: [KeyValue] -> [a] -> [a]
limit pm rec = case lookup "limit" pm of
                       Just x  -> case decimal x of 
                                   Right (x', _) -> take x' rec
                                   Left _ -> rec
                       Nothing -> rec

mkFilters :: FilterMap a -> [KeyValue] -> [Filter a]
mkFilters flt = map mkFilter
    where mkFilter (k, v) = case lookup k flt of
                             Nothing -> const True
                             Just ac -> match ac v

match :: Accessor a -> Value -> Filter a
match (AccessInt      get) arg = cmpNum arg . get                -- match a comparable Int (>100, <100, ..)
match (AccessText     get) arg = TL.isPrefixOf arg . get         -- match a text (or prefix thereof)
match (AccessMaybeInt get) arg = maybe False (cmpNum arg) . get  -- match a Maybe wrapped Int
match (AccessMonth    get) arg = cmpNum arg . month . ymd . get  -- match the month in a date
match (AccessYear     get) arg = cmpNum arg . year . ymd . get   -- match the year in a date

-- Helper functions

-- get year, month or day from date
ymd dt = toGregorian $ utctDay dt
year  (y,_,_) = fromInteger y
month (_,m,_) = m
day   (_,_,d) = d

-- turn a number string with optional compare operator into a compare function
cmpNum :: Value -> (Int -> Bool)
cmpNum = parse .  decimal . prepare 
    where parse (Right (x, c)) = cmp x c
          parse (Left   _    ) = const False
          cmp x ">" = (>x)
          cmp x "<" = (<x)
          cmp x  _  = (==x)
          prepare t = case TL.take 1 t of 
                        ">" -> TL.drop 1 t `mappend` ">"
                        "<" -> TL.drop 1 t `mappend` "<"
                        _   -> t
