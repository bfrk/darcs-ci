module Darcs.Test.Patch.Utils
    ( testConditional
    , testConditionalMaybe
    , testStringList
    , TestGenerator(..)
    , TestCondition(..)
    , TestCheck(..)
    , PropList
    , properties
    , testCases
    ) where

import Darcs.Prelude

import Data.Maybe ( fromMaybe )

import Test.Tasty ( TestTree, TestName )
import Test.Tasty.HUnit ( testCase )
import Test.Tasty.QuickCheck ( testProperty )
import Test.HUnit ( assertFailure )
import Test.QuickCheck ( Arbitrary, Testable, (==>) )

import Darcs.Test.Util.TestResult

-- | Turns a condition and a test function into a conditional quickcheck
--   property that can be run by test-framework.
testConditional
  :: (Arbitrary a, Show a, Testable prop) => TestName     -- ^ Test name
                                          -> (a -> Bool)  -- ^ Condition
                                          -> (a -> prop)  -- ^ Test function
                                          -> TestTree
testConditional name cond t = testProperty name t'
    where t' x = cond x ==> t x

testConditionalMaybe
  :: (Arbitrary a, Show a, Testable prop)
  => TestName -- ^ Test name
  -> (a -> Maybe Bool) -- ^ Condition
  -> (a -> prop) -- ^ Test function
  -> TestTree
testConditionalMaybe name cond t = testProperty name t'
  where
    cond' x =
      case cond x of
        Nothing -> False
        Just b -> b
    t' x = cond' x ==> t x

-- | Utility function to run old tests that return a list of error messages,
--   with the empty list meaning success.
testStringList :: String -> [String] -> TestTree
testStringList name test = testCase name $ mapM_ assertFailure test

-- | Run a test function on a set of data, using HUnit. The test function should
--   return @Nothing@ upon success and a @Just x@ upon failure.
testCases :: String             -- ^ The test name
          -> (a -> TestResult)  -- ^ The test function
          -> [a]                -- ^ The test data
          -> TestTree
testCases name test datas = testCase name (mapM_ (assertNotFailed . test) datas)

newtype TestGenerator thing gen =
  TestGenerator (forall t. (forall wX wY. thing wX wY -> t) -> (gen -> Maybe t))

newtype TestCondition thing =
  TestCondition (forall wX wY. thing wX wY -> Bool)

newtype TestCheck thing t =
  TestCheck (forall wX wY. thing wX wY -> t)

type PropList what gen = String -> TestGenerator what gen -> [TestTree]

properties :: forall thing gen. (Show gen, Arbitrary gen)
           => TestGenerator thing gen
           -> String -> String
           -> forall t. (Testable t) => [(String, TestCondition thing, TestCheck thing t)]
           -> [TestTree]
properties (TestGenerator gen) prefix genname tests =
  [cond name condition check | (name, condition, check) <- tests]
  where
    cond ::
         forall testable. (Testable testable)
      => String
      -> TestCondition thing
      -> TestCheck thing testable
      -> TestTree
    cond t (TestCondition c) (TestCheck p) =
      testConditional
        (prefix ++ " (" ++ genname ++ "): " ++ t)
        (fromMaybe False . gen c)
        (gen p)
