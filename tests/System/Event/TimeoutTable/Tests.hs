{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module System.Event.TimeoutTable.Tests (tests) where

import           Data.List (nubBy, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (isJust, maybe)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (addDays, fromGregorian)
import qualified System.Event.TimeoutTable as TT
import qualified System.Event.TimeoutTable.Internal as TT

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.QuickCheck (testProperty)
import           Test.QuickCheck (choose, (==>), Arbitrary(..), Property)

newtype UTM = UTM { unUTM :: UTCTime }
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------
-- quickcheck instance for UTCTime
instance Arbitrary UTM where
    arbitrary = do
        ndays <- choose (0,4)
        nsecs <- choose (0,86399)

        return $ UTM $ UTCTime (addDays ndays firstDay) (fromInteger nsecs)
      where
        firstDay = fromGregorian 2009 11 10

    coarbitrary = undefined


------------------------------------------------------------------------------
-- quickcheck instance for (UTM,Int,Int)

newtype TVal = TVal (UTM,Int,Int)
  deriving (Eq, Show)

unTVal :: TVal -> (UTCTime, Int, Int)
unTVal (TVal ((UTM tm), a, b)) = (tm, a, b)

unTVals :: [TVal] -> [(UTCTime, Int, Int)]
unTVals = map unTVal

instance Arbitrary TVal where
    arbitrary = do
        tm <- arbitrary
        i1 <- arbitrary
        i2 <- arbitrary

        return $ TVal (tm,i1,i2)

    coarbitrary = undefined


------------------------------------------------------------------------------
-- tests follow
------------------------------------------------------------------------------
tests :: Test.Framework.Test
tests = testGroup "System.Event.TimeoutTable" testlist


testlist :: [Test]
testlist = 
    [ testProperty "empty" prop_empty
    , testProperty "singleton" prop_singleton
    , testProperty "insert_delete" prop_insert_delete
    , testProperty "oldest" prop_oldest ]


prop_empty :: Bool
prop_empty = Map.null (TT._keySet tt) && Map.null (TT._timeSet tt)
  where
    tt = TT.empty


prop_singleton :: TVal -> Bool
prop_singleton (TVal (utm,k,v)) = member && p1 && p2
  where
    tm     = unUTM utm
    tt     = TT.insert tm k v TT.empty
    member = TT.member k tt
    oldest = TT.findOldest tt
    mbval  = TT.find k tt

    p1 = maybe False (\(tm',k',v') -> tm == tm' && k == k' && v == v') oldest
    p2 = maybe False (\(tm',v') -> tm == tm' && v == v') mbval


prop_insert_delete :: (TVal, [TVal]) -> Bool
prop_insert_delete (TVal ((UTM tm),k,v), tvals) =
    and [ member, not memberAfter, p1, p2 ]
  where
    vals                   = unTVals tvals
    startTable             = TT.fromList vals
    withElem               = TT.insert tm k v startTable
    afterDelete            = TT.delete k withElem
    member                 = TT.member k withElem
    mbval                  = TT.find k withElem

    memberAfter            = TT.member k afterDelete
    valAfter               = TT.find k afterDelete

    p1 = maybe False (\(tm',v') -> tm == tm' && v == v') mbval
    p2 = not $ isJust valAfter


prop_oldest :: [TVal] -> Property
prop_oldest tvals = (not $ null vals) ==> p1
  where
    tmpvals = unTVals tvals
    vals    = nubBy tEQ tmpvals
    sorted  = sortBy tCompare vals

    tEQ      (a,i,_) (b,j,_) = a == b || i == j
    tCompare (a,i,_) (b,j,_) = if a == b then compare i j else compare a b

    (tm,k,v) = head sorted
    tab      = TT.fromList vals

    mbOldest = TT.findOldest tab

    p1 = maybe False (\(tm',k',v') -> tm == tm' && k == k' && v == v') mbOldest
