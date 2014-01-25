module Control.Concurrent.TokenBucket
    ( TokenBucket
    , newTokenBucket
    , tokenBucketTryAlloc
    , tokenBucketTryAlloc1
    , tokenBucketWait
    ) where

-- import Utils

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)

newtype TokenBucket = TB (IORef TBData)

data TBData = TBData !Word64 !PosixTimeUsecs
              deriving Show

type PosixTimeUsecs = Word64

getTBData :: TokenBucket -> IO TBData
getTBData (TB lbd) = readIORef lbd

getPosixTimeUsecs :: IO PosixTimeUsecs
getPosixTimeUsecs = fmap (floor . (*1e6)) getPOSIXTime

newTokenBucket :: IO TokenBucket
newTokenBucket = do
    now <- getPosixTimeUsecs
    lbd <- newIORef $! TBData 0 now
    evaluate (TB lbd)

tokenBucketTryAlloc :: TokenBucket -> Word64 -> Word64 -> Word64 -> IO Bool
tokenBucketTryAlloc _ _  0 _ = return True -- infinitive rate, no-op
tokenBucketTryAlloc _ burst _ alloc | alloc > burst = return False
tokenBucketTryAlloc (TB lbref) burst invRate alloc = do
    now <- getPosixTimeUsecs
    atomicModifyIORef' lbref (go now)
  where
    go now (TBData lvl ts)
      | lvl'' > burst = (TBData lvl'  ts', False)
      | otherwise     = (TBData lvl'' ts', True)
      where
        lvl' = lvl ∸ dl
        (dl,dtRem) = dt `quotRem` invRate
        dt   = now ∸ ts
        ts'  = now ∸ dtRem

        lvl'' = lvl' ∔ alloc

-- | Returns 0 if succesful, or minimum usecs to wait till allocation /may/ succeed otherwise
tokenBucketTryAlloc1 :: TokenBucket -> Word64 -> Word64 -> IO Word64
tokenBucketTryAlloc1 _ _ 0 = return 0 -- infinite rate, no-op
tokenBucketTryAlloc1 (TB lbref) burst invRate = do
    now <- getPosixTimeUsecs
    atomicModifyIORef' lbref (go now)
  where
    go now (TBData lvl ts)
      | lvl'' > burst = (TBData lvl'  ts', invRate-dtRem)
      | otherwise     = (TBData lvl'' ts', 0)
      where
        lvl' = lvl ∸ dl
        (dl,dtRem) = dt `quotRem` invRate
        dt   = now ∸ ts
        ts'  = now ∸ dtRem

        -- tsRetry = (alloc∸?)*invRate ∸ dtRem

        lvl'' = lvl' ∔ 1


tokenBucketWait :: TokenBucket -> Word64 -> Word64 -> IO ()
tokenBucketWait tb burst invRate = do
    delay <- tokenBucketTryAlloc1 tb burst invRate
    unless (delay == 0) $ do
        -- logPrint $ "sleeping " ++ show delay ++ " usecs to satisfy rate-limit..."
        threadDelay (fromIntegral delay)
        tokenBucketWait tb burst invRate



(∸), (∔) :: Word64 -> Word64 -> Word64
x ∸ y = if x>y then x-y else 0
{-# INLINE (∸) #-}
x ∔ y = let s=x+y in if x <= s then s else maxBound
{-# INLINE (∔) #-}

{-

class exports.LazyBucket
  !(@size, @rate)->
    @time = Date.now ()
    @level = 0
  drop: (drop)->
    now = Date.now ()
    @level -= @rate * (now - @time)
    @time = now
    if @level < 0 then @level = 0
    @level += drop
    if @level > @size
      @level -= drop
      return false
    return true


-}
