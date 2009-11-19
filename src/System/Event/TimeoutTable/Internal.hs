{-# LANGUAGE BangPatterns #-}

module System.Event.TimeoutTable.Internal where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Format (formatTime)
import           Prelude hiding (null)
import           System.Locale (defaultTimeLocale)


------------------------------------------------------------------------------
-- Maybe too expensive, replace with some cheaper hi-res timer?
type TimeRep = UTCTime


------------------------------------------------------------------------------
{-|

A TimeoutTable is a key-value mapping with an associated timeout value. You
can:

  * look up, query, delete, or modify values by key

  * find the oldest (i.e. first to expire) entry in the table

  * update the timeout value for a key

-}

data TimeoutTable k a = TimeoutTable
    { _keySet  :: !(Map k (TimeRep, a))
    , _timeSet :: !(Map TimeRep [k]) }


------------------------------------------------------------------------------
instance (Show k, Show a) => Show (TimeoutTable k a) where
    show (TimeoutTable ks _) = "<TimeoutTable (" ++ show (fmap f ks) ++ ")>"

      where
        f :: (Show a) => (TimeRep, a) -> String
        f (tr,a) = "(" ++ showUTC tr ++ "," ++ show a ++ ")"

        showUTC = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"



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
