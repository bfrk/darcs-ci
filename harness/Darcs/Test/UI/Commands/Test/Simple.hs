module Darcs.Test.UI.Commands.Test.Simple ( testSuite ) where

import Darcs.Prelude
import qualified Darcs.Util.IndexedMonad as Indexed

import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Set ( Origin )
import Darcs.Patch.Witnesses.Ordered
  ( RL(..), FL(..), consGapFL, reverseFL, mapFL, (:>)(..) )
import Darcs.Patch.Witnesses.Sealed
  ( Sealed(..), unseal, mapSeal, Sealed2(..)
  , FreeLeft, unFreeLeft, Gap(..)
  )
import Darcs.Patch.Witnesses.Show ( Show2(..) )

import qualified Darcs.UI.Options.All as O
import Darcs.UI.Commands.Test.Impl
  ( TestRunner(..), TestResult(..), TestResultValid(..), TestFailure(..)
  , runStrategy, StrategyResultRaw(..)
  , PatchSeq(..), patchTreeToFL
  )

import Darcs.Test.UI.Commands.Test.IndexedApply ( IndexedApply(..) )

import Data.Constraint ( Dict(..) )
import Test.Tasty.HUnit ( testCase )
import Test.Tasty.QuickCheck ( QuickCheckMaxSize(..), testProperty )
import Test.Tasty ( TestTree, adjustOption, testGroup )
import Test.HUnit ( assertEqual )
import Test.QuickCheck ( Arbitrary(..), Gen, Property, property, Discard(..), forAll, forAllShrink )
import Test.QuickCheck.Gen ( listOf, listOf1, frequency, elements )

testSuite :: TestTree
testSuite =
  testGroup "Darcs.UI.Commands.Test.Simple"
    [ testGroup "Generic test cases" $ map genericTestCases [O.Linear, O.Bisect, O.Backoff]
    , testGroup "Randomised tests of linear" [linearRandomised]
    , testGroup "Randomised tests against linear" $ map genericRandomised [O.Bisect, O.Backoff]
    ]

genericTestCases :: O.TestStrategy -> TestTree
genericTestCases testStrategy =
  testGroup (show testStrategy) $ map (expectedResult testStrategy)
    [ ("Sequence ending in success", ((U, [S]), NoFailureOnHead))
    , ("Sequence with no passes (1)", ((U, [F]), NoPasses))
    , ("Sequence with no passes (2)", ((F, [F]), NoPasses))
    , ("Sequence with simple failure (1)", ((S, [F]), Blame [0]))
    , ("Sequence with simple failure (2)", ((S, [S,S,F,F]), Blame [2]))
    , ("Sequence with untestable states", ((S, [S,S,U,U,F,F]), Blame [2,3,4]))
    , ("Sequence with initial untestable states", ((U, [U,U,U,U,U,U,U,S,S,U,U,F,F]), Blame [9,10,11]))
    ]

type ExpectedResult = ((TestingState, [TestingState]), StrategyResultRaw [Int])

expectedResult :: O.TestStrategy -> (String, ExpectedResult) -> TestTree
expectedResult testStrategy (testName, (testDetails, expectedTestResult)) =
  testCase testName $ do
  -- whether we try to shrink or not is irrelevant as nothing will commute
    let result = runStrategyOn testStrategy O.NoShrinkFailure testDetails
    assertEqual "Unexpected result" expectedTestResult result

genericRandomised :: O.TestStrategy -> TestTree
genericRandomised testStrategy =
  testGroup (show testStrategy)
    [ testProperty "simple sequence" (simpleSequence testStrategy)
    , adjustOption (\(QuickCheckMaxSize n) -> QuickCheckMaxSize (n `div` 5)) $
      testProperty "multi sequence" (multiSequence testStrategy)
    ]

linearRandomised :: TestTree
linearRandomised =
  testGroup (show O.Linear)
    [ testProperty "blame is found when possible" (findBlame O.Linear)
    ]

simpleSequence :: O.TestStrategy -> SimpleSequenceDefinition -> Bool
simpleSequence testStrategy sequenceDef =
  let s = ssdToTestDetails sequenceDef in
  runStrategyOn O.Linear O.NoShrinkFailure s == runStrategyOn testStrategy O.NoShrinkFailure s

-- tests that if we stick multiple sequences each with a single "blame sequence"
-- together, the strategy finds one of those sequences
multiSequence :: O.TestStrategy -> Property
multiSequence testStrategy =
  forAllShrink (listOf1 arbitraryBlameSSD) shrink $ \sequenceDefs ->
    let
      allTestDetails = map ssdToTestDetails sequenceDefs
      mergeDetails (i1, s1) (i2, s2) = (i1, s1 ++ [i2] ++ s2)
      adjustedResults _ [] = Just []
      adjustedResults n (x:xs) =
        case (runStrategyOn O.Linear O.NoShrinkFailure x, adjustedResults (n + 1 + length (snd x)) xs) of
          (Blame ps, Just ys) -> Just (Blame (map (+n) ps) : ys)
          _ -> Nothing
    in
      case adjustedResults 0 allTestDetails of
        Nothing -> property Discard
        Just [] -> property Discard
        Just res -> property $
                       runStrategyOn testStrategy O.NoShrinkFailure (foldr1 mergeDetails allTestDetails) `elem` res

findBlame :: O.TestStrategy -> Property
findBlame testStrategy =
  forAll arbitraryBlameSSD $ \s ->
    case runStrategyOn testStrategy O.NoShrinkFailure (ssdToTestDetails s) of
      Blame _ -> True
      _ -> False

-- a sequence of test results guaranteed to be monotonic in success/failure
data SimpleSequenceDefinition =
  SimpleSequenceDefinition
  { successPart :: [TestingState] -- contains either S or U
  , middlePart  :: TestingState   -- either S, F or U - exists to guarantee the joined list is non-empty
  , failurePart :: [TestingState] -- contains either F or U
  }
  deriving Show

ssdToTestDetails :: SimpleSequenceDefinition -> (TestingState, [TestingState])
ssdToTestDetails ssd =
  case successPart ssd ++ [middlePart ssd] ++ failurePart ssd of
    x:xs -> (x, xs)
    _ -> error "internal error: impossible empty list in ssdToTestDetails"

instance Arbitrary SimpleSequenceDefinition where
  arbitrary = do
    s <- listOf (frequency [(1, r S), (3, r U)])
    m <- frequency [(1, r S), (5, r U), (1, r F)]
    f <- listOf (frequency [(1, r F), (3, r U)])
    return $ SimpleSequenceDefinition s m f
   where r = return

  shrink (SimpleSequenceDefinition s m f) =
    map (SimpleSequenceDefinition s m) (shrink f) ++
    map (\s' -> SimpleSequenceDefinition s' m f) (shrink s)

-- an arbitrary that's guaranteed to produce a sequence that results in Blame xxx
arbitraryBlameSSD :: Gen SimpleSequenceDefinition
arbitraryBlameSSD = do
  s1 <- listOf (frequency [(1, r S), (3, r U)])
  s2 <- listOf (frequency [(1, r S), (3, r U)])
  m <- frequency [(1, r S), (5, r U), (1, r F)]
  f <- listOf (frequency [(1, r F), (3, r U)])
  return $ SimpleSequenceDefinition (s1 ++ [S] ++ s2) m (f ++ [F])
 where r = return

instance Arbitrary TestingState where
  arbitrary = elements [S, U, F]
  shrink _ = []

data TestingState = S | U | F
   deriving (Eq, Show)

data TrivialPatch wX wY = TrivialPatch Int TestingState TestingState
  deriving Show

instance Show2 TrivialPatch where
  showDict2 = Dict

runStrategyOn :: O.TestStrategy -> O.ShrinkFailure -> (TestingState, [TestingState]) -> StrategyResultRaw [Int]
runStrategyOn testStrategy shrinkFailure (initialState, patchStates) =
  let finalState = last (initialState:patchStates) in
  unseal (fmap toPatchNums . fst . flip runTestingMonad finalState . runStrategy testStrategy shrinkFailure)
         (genPatchSequence initialState patchStates)

toPatchNums :: (Sealed2 (PatchSeq TrivialPatch)) -> [Int]
toPatchNums (Sealed2 ps) = mapFL (\(TrivialPatch n _ _) -> n) (patchTreeToFL ps)

genPatchSequence :: TestingState -> [TestingState] -> Sealed (RL TrivialPatch Origin)
genPatchSequence initialState patchStates =
  mapSeal reverseFL $ unFreeLeft $ doGen 0 initialState patchStates
  where
    doGen :: Int -> TestingState -> [TestingState] -> FreeLeft (FL TrivialPatch)
    doGen _ _ [] = emptyGap NilFL
    doGen n startingState (nextState:states) =
      consGapFL (TrivialPatch n startingState nextState) (doGen (n+1) nextState states)

instance Invert TrivialPatch where
  invert (TrivialPatch num ov nv) = TrivialPatch num nv ov

instance Commute TrivialPatch where
  commute (_ :> _) = Nothing

data TestingMonad wX wY a = TestingMonad { runTestingMonad :: TestingState -> (a, TestingState) }

instance Indexed.Monad TestingMonad where
  return v = TestingMonad (\n -> (v, n))
  m >>= f = TestingMonad (\n1 -> let (a, n2) = runTestingMonad m n1 in runTestingMonad (f a) n2)
  m1 >> m2 = TestingMonad (\n1 -> let (_, n2) = runTestingMonad m1 n1 in runTestingMonad m2 n2)

toTestResult :: TestingState -> TestResult wX
toTestResult S = Testable Success
toTestResult U = Untestable
toTestResult F = Testable (Failure (TestFailure 1))

instance IndexedApply TrivialPatch where
  type ApplyState TrivialPatch = TestingMonad
  apply (TrivialPatch num old new) =
    TestingMonad $ \st ->
      if st == old
        then ((), new)
        else error $ "state mismatch for patch " ++ show num
                          ++ ", expected " ++ show old ++ ", got " ++ show st

  unapply (TrivialPatch num old new) =
    TestingMonad $ \st ->
      if st == new
        then ((), old)
        else error $ "state mismatch for patch " ++ show num
                          ++ ", expected " ++ show new ++ ", got " ++ show st

instance TestRunner TestingMonad where
  type ApplyPatchReqs TestingMonad p = (IndexedApply p, ApplyState p ~ TestingMonad)
  type DisplayPatchReqs TestingMonad p = p ~ TrivialPatch

  writeMsg _ = Indexed.return ()
  mentionPatch _ = Indexed.return ()
  finishedTesting v = TestingMonad (\_ -> (v, error "something tried to read final testing state"))

  getCurrentTestResult = TestingMonad (\n -> (toTestResult n, n))

  applyPatch = apply
  unapplyPatch = unapply
