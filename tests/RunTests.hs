module Main where

import qualified System.Event.Array.Tests

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain tests
  where tests = [System.Event.Array.Tests.tests]
