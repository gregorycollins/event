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
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Format (formatTime)
import           Prelude hiding (null)
import           System.Locale (defaultTimeLocale)

------------------------------------------------------------------------------


-- Maybe too expensive, replace with some cheaper hi-res timer?
type TimeRep = UTCTime

data TimeoutTable k a = TimeoutTable
    { _keySet  :: !(Map k (TimeRep, a))
    , _timeSet :: !(Map TimeRep [k]) }


instance (Show k, Show a) => Show (TimeoutTable k a) where
    show (TimeoutTable ks _) = "<TimeoutTable (" ++ show (fmap f ks) ++ ")>"

      where
        f :: (Show a) => (TimeRep, a) -> String
        f (tr,a) = "(" ++ showUTC tr ++ "," ++ show a ++ ")"

        showUTC = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"


empty :: TimeoutTable k a
empty = TimeoutTable Map.empty Map.empty


null :: TimeoutTable k a -> Bool
null (TimeoutTable k _) = Map.null k


findOldest :: (Ord k) => TimeoutTable k a -> Maybe (TimeRep, k, a)
findOldest (TimeoutTable keys times) | Map.null keys = Nothing
                                     | otherwise = if List.null l
                                                     then Nothing
                                                     else Just (t, hd, snd el)
  where
    (t,l) = Map.findMin times
    hd    = head l
    el    = fromJust $ Map.lookup hd keys



find :: (Ord k) => k -> TimeoutTable k a -> Maybe (TimeRep, a)
find k tab = Map.lookup k $ _keySet tab


member :: (Ord k) => k -> TimeoutTable k a -> Bool
member k tab = Map.member k $ _keySet tab


insert :: (Ord k) => TimeRep -> k -> a -> TimeoutTable k a -> TimeoutTable k a
insert !tm !k !v !tab = TimeoutTable ks' ts'
  where
     !tab'       = delete k tab
     !ks         = _keySet tab'
     !ts         = _timeSet tab'

     !ks'        = Map.insert k (tm,v) ks
     !ts'        = Map.insertWith' ((:) . head) tm [k] ts


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



------------------------------------------------------------------------------
-- internal functions follow
------------------------------------------------------------------------------

removeFromTimeSet :: (Ord k) =>
                     TimeRep
                  -> k
                  -> Map TimeRep [k]
                  -> Map TimeRep [k]
removeFromTimeSet tm k ts = maybe ts killIt mbOld
  where
    mbOld     = Map.lookup tm ts
    killIt ks = if List.null ks'
                  then Map.delete tm ts
                  else Map.insert tm ks' ts
      where
        ks' = List.delete k ks
