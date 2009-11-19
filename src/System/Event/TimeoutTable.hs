{-# LANGUAGE BangPatterns #-}

{-|

FIXME: add description

-}

module System.Event.TimeoutTable
  ( TimeoutTable
  , TimeRep
  , empty
  , null
  , findOldest
  , find
  , member
  , insert
  , delete
  , update
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Prelude hiding (null)

import           System.Event.TimeoutTable.Internal

------------------------------------------------------------------------------
{-| An empty TimeoutTable. -}
empty :: TimeoutTable k a
empty = TimeoutTable Map.empty Map.empty


{-| Returns True if the table is empty. -}
null :: TimeoutTable k a -> Bool
null (TimeoutTable k _) = Map.null k


{-| Find the entry in the table with the first (oldest) expiry time. -}
findOldest :: (Ord k) => TimeoutTable k a -> Maybe (TimeRep, k, a)
findOldest (TimeoutTable keys times) | Map.null keys = Nothing
                                     | otherwise = if List.null l
                                                     then Nothing
                                                     else Just (t, hd, snd el)
  where
    (t,l) = Map.findMin times
    hd    = head l
    el    = fromJust $ Map.lookup hd keys


{-| Lookup a value by key. -}
find :: (Ord k) => k -> TimeoutTable k a -> Maybe (TimeRep, a)
find k tab = Map.lookup k $ _keySet tab


{-| Is the given key a member of the table? -}
member :: (Ord k) => k -> TimeoutTable k a -> Bool
member k tab = Map.member k $ _keySet tab


{-| Add a new key-value-timeout mapping to the table. -}
insert :: (Ord k) => TimeRep -> k -> a -> TimeoutTable k a -> TimeoutTable k a
insert !tm !k !v !tab = TimeoutTable ks' ts'
  where
     !tab'       = delete k tab
     !ks         = _keySet tab'
     !ts         = _timeSet tab'

     !ks'        = Map.insert k (tm,v) ks
     !ts'        = Map.insertWith' ((:) . head) tm [k] ts


{-| Delete a key-value mapping from the table. -}
delete :: (Ord k) => k -> TimeoutTable k a -> TimeoutTable k a
delete !k !tab = maybe tab killIt mbTm
  where
    !ks            = _keySet tab
    !ts            = _timeSet tab
    !mbTm          = Map.lookup k ks

    killIt (!tm,_) = TimeoutTable ks' ts'
      where
        !ks'  = Map.delete k ks
        !ts'  = removeFromTimeSet tm k ts



{-| Update the timeout value for a key in the table. -}
update :: (Ord k) => k -> TimeRep -> TimeoutTable k a -> TimeoutTable k a
update !k !tm !tab = maybe tab updateIt mbTm
  where
    !ks   = _keySet tab
    !ts   = _timeSet tab
    !mbTm = Map.lookup k ks

    updateIt (!oldTm, !v) = TimeoutTable ks' ts''
      where
        !ks'  = Map.insert k (tm,v) ks
        !ts'  = removeFromTimeSet oldTm k ts
        !ts'' = Map.insertWith' ((:) . head) tm [k] ts'
