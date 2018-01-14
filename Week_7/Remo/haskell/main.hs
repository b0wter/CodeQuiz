{-# LANGUAGE OverloadedStrings #-}

import Api
import System.IO
import System.Exit
import System.Environment
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Control.Logging as Log
import qualified Data.ByteString.Lazy.Char8 as L8

main = Log.withStderrLogging $ do
    getArgs >>= parse

run s = readEntity s >>= getter 

parse ["-h"]       = usage >> exit
parse ("-h":_)     = usage >> exit
parse ["-p", s, f] = withFile f WriteMode (run s encodePretty)
parse ["-p", s]    = run s encodePretty stdout
parse [s, f]       = withFile f WriteMode (run s encode)
parse [s]          = run s encode stdout
parse []           = usage >> exit
parse _            = usage >> exit

readEntity s = return (read s :: RecordName)

getter Foo     encoder hdl = Log.log "Loading ..." >> getAll Foo >>= (return . encoder) >>= L8.hPutStrLn hdl
getter AEntity encoder hdl = Log.log "Loading ..." >> getAll AEntity >>= (return . encoder) >>= L8.hPutStrLn hdl
getter BEntity encoder hdl = Log.log "Loading ..." >> getAll BEntity >>= (return . encoder) >>= L8.hPutStrLn hdl
getter _       _       _   = Log.log "invalid entity"

exit = exitWith ExitSuccess
usage = getProgName >>= putStr >> putStrLn " [-h] [-p] ENTITY [OUTFILE]"
