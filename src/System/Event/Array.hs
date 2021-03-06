{-# LANGUAGE CPP #-}

module System.Event.Array
    ( Array,
      empty,
      new,
      length,
      capacity,
      unsafeRead,
      unsafeWrite,
      unsafeLoad,
      ensureCapacity,
      useAsPtr,
      snoc,
      mapM_
    ) where

import Control.Monad (when)
import Data.IORef
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (length, mapM_)

#define BOUNDS_CHECKING 1

#if defined(BOUNDS_CHECKING)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
#define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("System.Event.Array." ++ (_func_) ++ ": bounds error, index " ++ show (_k_) ++ ", capacity " ++ show (_len_)) else
#else
#define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

-- Invariant: size <= capacity
newtype Array a = Array (IORef (AC a))

-- The actual array content.
data AC a = AC
    !(Ptr a)  -- Elements
    !Int      -- Number of elements (length)
    !Int      -- Maximum number of elements (capacity)

empty :: IO (Array a)
empty = fmap Array (newIORef (AC nullPtr 0 0))

new :: Storable a => Int -> IO (Array a)
new c = do
    es <- mallocArray cap
    fmap Array (newIORef (AC es 0 cap))
  where
    cap = firstPowerOf2 c

length :: Array a -> IO Int
length (Array ref) = do
    AC _ len _ <- readIORef ref
    return len

capacity :: Array a -> IO Int
capacity (Array ref) = do
    AC _ _ cap <- readIORef ref
    return cap

unsafeRead :: Storable a => Array a -> Int -> IO a
unsafeRead (Array ref) ix = do
    AC es _ cap <- readIORef ref
    CHECK_BOUNDS("unsafeRead",cap,ix)
      peekElemOff es ix

unsafeWrite :: Storable a => Array a -> Int -> a -> IO ()
unsafeWrite (Array ref) ix a = do
    AC es _ cap <- readIORef ref
    CHECK_BOUNDS("unsafeWrite",cap,ix)
      pokeElemOff es ix a

unsafeLoad :: Storable a => Array a -> (Ptr a -> Int -> IO Int) -> IO Int
unsafeLoad (Array ref) load = do
    AC es _ cap <- readIORef ref
    len' <- load es cap
    writeIORef ref (AC es len' cap)
    return len'

ensureCapacity :: Storable a => Array a -> Int -> IO ()
ensureCapacity (Array ref) c = do
    AC es len cap <- readIORef ref
    when (c > cap) $ do
        es' <- reallocArray es cap'
        writeIORef ref (AC es' len cap')
  where
    cap' = firstPowerOf2 c

useAsPtr :: Array a -> (Ptr a -> Int -> IO b) -> IO b
useAsPtr (Array ref) f = do
    AC es _ cap <- readIORef ref
    f es cap

snoc :: Storable a => Array a -> a -> IO ()
snoc arr@(Array ref) e = do
    len <- length arr
    let len' = succ len
    ensureCapacity arr len'
    unsafeWrite arr len e
    AC es _ cap <- readIORef ref
    writeIORef ref (AC es len' cap)

mapM_ :: Storable a => Array a -> (a -> IO ()) -> IO ()
mapM_ ary g = mapHack ary g undefined
  where
    mapHack :: Storable b => Array b -> (b -> IO ()) -> b -> IO ()
    mapHack (Array ref) f dummy = do
      AC es len _ <- readIORef ref
      let size = sizeOf dummy
          offset = len * size
      let loop n
              | n >= offset = return ()
              | otherwise = do
                    f =<< peek (es `plusPtr` n)
                    loop (n + size)
      loop 0

firstPowerOf2 :: Int -> Int
firstPowerOf2 n
    | n <= 0    = 0
    | otherwise = 2^p
  where p = (ceiling . logBase (2 :: Double) . realToFrac) n :: Int
