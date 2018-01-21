{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Iowa
import Filter
import Response
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)

maxNumRows = 25000

main = do 
    records <- testLoadIowaFile maxNumRows iowaFilePath 
    scotty 9000 $ do
       get "/api" $ do
            pm <- params
            tm <- liftIO getCurrentTime
            let results = filterAndLimit pm records
            json $ mkResponse tm results

