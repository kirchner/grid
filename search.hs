#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "pkgs.ghc.withPackages(p: with p; [ stm ])"

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Map (Map)
import System.IO
import System.Process
import Text.Printf

import qualified Data.Map as Map


-- Number of concurrent threads to spawn:
j = 8

-- Length of inputs, ie. "up", "down", …:
k = 8


main = do
    queueM <- newTMVarIO (makeInput k)
    reportM <- newTMVarIO Map.empty
    threadM <- newTMVarIO 0
    sequence $ replicate j $ forkIO $ eval threadM queueM reportM
    loop threadM reportM
  where
    makeInput 0 = [[]]
    makeInput n = [ i : is | i <- inputs, is <- makeInput (n-1) ]
    inputs = [ "up", "down", "left", "right" ]
    once m = m
    loop threadM reportM = do
        n <- atomically $ do
            n <- takeTMVar threadM
            putTMVar threadM n
            return n
        if n >= j then do
            result <- atomically $ do
                result <- takeTMVar reportM
                putTMVar reportM result
                return (result :: Map String Int)
            printf "\n\n"
            mapM (uncurry (printf "%s: %d\n")) (Map.toList result)
        else do
            result <- atomically $ do
                result <- takeTMVar reportM
                putTMVar reportM result
                return (result :: Map String Int)
            (\won lost fail ->
                printf "\r#WON: %.6d, #LOST: %.6d, #FAIL: %.6d, COMPLETE: %.1f%%"
                    won
                    lost
                    fail
                    (100*fromIntegral (won + lost + fail) / fromIntegral (4^k) :: Float))
                (Map.findWithDefault 0 "WON" result)
                (Map.findWithDefault 0 "LOST" result)
                (Map.findWithDefault 0 "FAIL" result)
            hFlush stdout
            threadDelay (10^6)
            loop threadM reportM


eval threadM queueM reportM = do
    loop
  where
    loop =
        forever $ do
            input <- getInput
            case input of
                Just input -> do
                    result <- simulate input
                    updateReport result
                    loop
                Nothing ->
                    die
    die = atomically $ do
        n <- takeTMVar threadM
        putTMVar threadM (n+1)
    updateReport result = atomically $ do
        r <- takeTMVar reportM
        putTMVar reportM (Map.alter (Just . maybe 0 ((+) 1)) result r)
    getInput = atomically $ do
        iss <- takeTMVar queueM
        case iss of
            [] -> do
                putTMVar queueM iss
                return Nothing
            (i:is) -> do
                putTMVar queueM is
                return (Just i)


simulate i =
    fmap (\s -> take (length s - 1) s) $
    readProcess "node" ["./runner.js", "elm.js"] (intercalate "\n" i ++ "\n")
