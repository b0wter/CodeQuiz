module Main where

import Life
import System.Random
import Control.Exception
import Control.Concurrent (threadDelay)

import System.Console.ANSI
import Options

main :: IO ()
main = do
    (Options width height fps)  <- getOpts
    setTitle "Game of Life"
    clearScreen
    hideCursor
    game fps height width showWorld `catch` \UserInterrupt -> exitGame

game fps h w display = do
    rng <- getStdGen
    let world = mkRandWorld rng h w
    mapM_ (frame fps . display) (generate world)
    where generate = iterate generation

exitGame = do
    showCursor

frame speed content = do
    draw content
    threadDelay delay
    where draw str = setCursorPosition 0 0 >> putStrLn str
          delay    = 1000000 `div` speed
