{-# LANGUAGE OverloadedStrings #-}

module Filter (filterRecords, limit, Accessor(..), FilterList) where

import Web.Scotty (Param)
import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Text.Lazy.Read (decimal)
import qualified Data.Text.Lazy as TL

-- Wrapper for accessor functions
data Accessor a = AccessText     (a -> TL.Text) 
                | AccessInt      (a -> Int)
                | AccessMaybeInt (a -> (Maybe Int))
                | AccessMonth    (a -> UTCTime)
                | AccessYear     (a -> UTCTime)

type FilterList a = [(TL.Text, Accessor a)]

filterRecords :: FilterList a -> [Param] -> [a] -> [a]
filterRecords flt pm rec = foldr filter rec . mkFilters flt $ pm 

limit :: [Param] -> [a] -> [a]
limit pm rec = case lookup "limit" pm of
                       Just (x) -> case decimal x of 
                                   Right (x', _) -> take x' rec
                                   Left _ -> rec
                       Nothing -> rec

mkFilters :: FilterList a -> [Param] -> [(a -> Bool)]
mkFilters flt = map mkFilter
    where mkFilter (k, v) = case lookup k flt of
                             Nothing -> const True
                             Just ac -> match ac v

match :: Accessor a -> TL.Text -> (a -> Bool)
match (AccessInt      get) arg = mkCompare arg . get                -- match a comparable Int (>100, <100, ..)
match (AccessText     get) arg = TL.isPrefixOf (takeText arg) . get -- match a text (or prefix thereof)
match (AccessMaybeInt get) arg = maybe False (mkCompare arg) . get  -- match a Maybe wrapped Int
match (AccessMonth    get) arg = mkCompare arg . month . ymd . get  -- match the month in a date
match (AccessYear     get) arg = mkCompare arg . year . ymd . get   -- match the year in a date

-- Helper functions

ymd dt = toGregorian $ utctDay dt
year  (y,_,_) = fromInteger y
month (_,m,_) = m
day   (_,_,d) = d

takeNum = decimal . prepComparable
takeText = id

mkCompare :: TL.Text -> (Int -> Bool)
mkCompare v = parse $ takeNum v 
    where parse (Right (x, c)) = cmp x c
          parse (Left   _    ) = const False
          cmp x ">" = (>x)
          cmp x "<" = (<x)
          cmp x  _  = (==x)

-- convert from ">9000" to "9000>"
prepComparable t = case TL.take 1 t of 
                    ">" -> (TL.drop 1 t) `mappend` ">"
                    "<" -> (TL.drop 1 t) `mappend` "<"
                    otherwise -> t

