{-# LANGUAGE OverloadedStrings #-}

module Filter (filterAndLimit) where

import Iowa
import CustomFields
import Web.Scotty
import Data.Time.Clock
import Data.Time.Calendar (toGregorian)
import Data.Text.Lazy.Read (decimal)
import qualified Data.Text.Lazy as TL

matchRecord ("fips", v) = compareText fips v
matchRecord ("county", v) = compareText county v
matchRecord ("grandTotal", v) = compareNumber grandTotal v

matchRecord ("democratActive", v) = compareMaybeNumber democratActive v
matchRecord ("republicanActive", v) = compareMaybeNumber republicanActive v
matchRecord ("libertarianActive", v) = compareMaybeNumber libertarianActive v
matchRecord ("noPartyActive", v) = compareMaybeNumber noPartyActive v
matchRecord ("otherActive", v) = compareMaybeNumber otherActive v
matchRecord ("totalActive", v) = compareMaybeNumber totalActive v 

matchRecord ("democratInactive", v) = compareMaybeNumber democratInactive v
matchRecord ("republicanInactive", v) = compareMaybeNumber republicanInactive v
matchRecord ("libertarianInactive", v) = compareMaybeNumber libertarianInactive v
matchRecord ("noPartyInactive", v) = compareMaybeNumber noPartyInactive v
matchRecord ("otherInactive", v) = compareMaybeNumber otherInactive v
matchRecord ("totalInactive", v) = compareMaybeNumber totalInactive v 

matchRecord ("month", v) = compareMonth date v
matchRecord ("year", v) = compareYear date v
matchRecord _ = const True

filterAndLimit :: [Param] -> [VoterRegistration] -> [VoterRegistration]
filterAndLimit pm = limit pm . filterRecords pm

filterRecords :: [Param] -> [VoterRegistration] -> [VoterRegistration]
filterRecords pm rec = foldr filter rec . paramToFilter $ pm 

limit :: [Param] -> [VoterRegistration] -> [VoterRegistration]
limit pm rec = case lookup "limit" pm of
                       Just (x) -> case decimal x of 
                                   Right (x', _) -> take x' rec
                                   Left _ -> rec
                       Nothing -> rec

paramToFilter :: [Param] -> [VoterRegistration -> Bool]
paramToFilter = map matchRecord


takeNum = decimal . prepComparable
takeText = id

getCompare v = case takeNum v of 
                Right (x, cmp) -> case cmp of 
                                  ">" -> (>x)
                                  "<" -> (<x)
                                  otherwise -> (==x)
                Left _ -> const False

compareNumber get arg = getCompare arg . get 

compareText get arg = TL.isPrefixOf (takeText arg) . get

compareMaybeNumber get arg = cmpHelp . get
             where cmpHelp Nothing  = False
                   cmpHelp (Just g) = getCompare arg g

compareMonth get arg = cmp arg . get
       where cmp s x = getCompare s $ getMonth x
             getMonth dt = let (_, m, _) = toGregorian $ utctDay dt in m

compareYear get arg = cmp arg . get
       where cmp s x = getCompare s $ getYear x
             getYear dt = let (y, _, _) = toGregorian $ utctDay dt in fromInteger y

-- convert from ">9000" to "9000>"
prepComparable t = case TL.take 1 t of 
                 ">" -> (TL.drop 1 t) `mappend` ">"
                 "<" -> (TL.drop 1 t) `mappend` "<"
                 otherwise -> t

