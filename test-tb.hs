{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.TokenBucket
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit
import Data.Word

getPosixTime :: IO Double
getPosixTime = fmap realToFrac getPOSIXTime

toInvRate :: Double -> Word64
toInvRate r = round (1e6 / r)

timeIO :: IO a -> IO (Double, a)
timeIO act = do
    ts0 <- getPosixTime
    res <- act
    ts1 <- getPosixTime
    dt <- evaluate (ts1-ts0)
    return (dt,res)

timeIO_ :: IO a -> IO Double
timeIO_ = fmap fst . timeIO

main :: IO ()
main = runInUnboundThread $ do
    putStrLn "testing tocket-bucket..."

    !tb <- newTokenBucket

    replicateM_ 3 $ do
        check tb 10 10.0
        check tb 20 20.0
        check tb 50 50.0
        check tb 100 100.0
        check tb 200 200.0
        check tb 500 500.0
        check tb 1000 1000.0
        putStrLn "============================================="

  where
    check :: TokenBucket -> Int -> Double -> IO ()
    check tb n rate = do
        -- threadDelay 100000
        putStrLn $ "running "++show n++"+1 iterations with "++show rate++" Hz rate-limit..."
        dt <- timeIO_ (replicateM_ (n+1) $ (tokenBucketWait tb 1 (toInvRate rate)))
        let rate' = fromIntegral n/dt
        unless (rate' <= rate) $ do
            putStrLn $ "...FAILED! (effective rate was " ++ show rate' ++ " Hz)"
            exitFailure
        putStrLn $ "...PASSED (effective rate was " ++ show rate' ++ " Hz)"
