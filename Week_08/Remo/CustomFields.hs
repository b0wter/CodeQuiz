module CustomFields where

-- Custom field conversion

import Control.Monad
import Data.Csv
import Data.Time (defaultTimeLocale)
import Data.Time.Clock
import Data.Time.Format (parseTimeM)
import qualified Data.ByteString.Char8 as S8

dateTimeFmt = "%m/%d/%Y %r"
parseDateTime s = parseTimeM True defaultTimeLocale dateTimeFmt s :: Maybe UTCTime

instance FromField UTCTime where
    parseField s = case parseDateTime (S8.unpack s) of
                    Nothing -> mzero
                    Just tm -> pure tm

