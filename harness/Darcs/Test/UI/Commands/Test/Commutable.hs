module Darcs.Test.UI.Commands.Test.Commutable ( testSuite ) where

import Darcs.Prelude
import qualified Darcs.Util.IndexedMonad as Indexed

import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Set ( Origin )
import Darcs.Patch.Witnesses.Ordered
  ( (:>)(..) , FL(..) , RL(..) , consGapFL , mapFL , reverseFL )
import Darcs.Patch.Witnesses.Sealed
  ( Sealed(..), unseal, mapSeal, Sealed2(..)
  , FreeLeft, unFreeLeft, Gap(..)
  )

import Darcs.UI.Commands.Test.Impl
  ( TestRunner(..), TestResult(..), TestResultValid(..), TestFailure(..)
  , runStrategy, StrategyResultRaw(..)
  , PatchSeq(..), patchTreeToFL
  )
import qualified Darcs.UI.Options.All as O

import Darcs.Test.UI.Commands.Test.IndexedApply ( IndexedApply(..) )

import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( assertEqual )

testSuite :: Test
testSuite =
  testGroup "Darcs.UI.Commands.Test.Commutable"
    [ testGroup "Generic test cases" $ map genericTestCases [O.Linear, O.Bisect, O.Backoff]
    ]


genericTestCases :: O.TestStrategy -> Test
genericTestCases testStrategy =
  testGroup (show testStrategy) $ map (expectedResult testStrategy)
    [ ("Unminimisable sequence",
           ([StdDeps CreateTest, StdDeps BreakTest],
            Blame [1],
            Blame [1]
           )
      )
    , ("Simple minimisation",
           ([StdDeps CreateTest, StdDeps BreakCompile, StdDeps BreakTest, StdDeps FixCompile],
            Blame [1,2,3],
            Blame [2])
      )
    , ("Longer minimisation",
           ([ StdDeps CreateTest, StdDeps BreakCompile, StdDeps Irrelevant, StdDeps Irrelevant
            , StdDeps BreakTest, StdDeps Irrelevant, StdDeps Irrelevant, StdDeps FixCompile
            ],
            Blame [1..7],
            Blame [4])
      )
    , ("Simple internal dependency",
           ([ StdDeps CreateTest
            , ExtraDeps 1 [] BreakCompile
            , ExtraDeps 2 [1] Irrelevant
            , StdDeps BreakTest
            , StdDeps FixCompile
            ],
            Blame [1..4],
            Blame [3])
      )
    , ("Simple internal dependency with extra patch",
           ([ StdDeps CreateTest
            , ExtraDeps 1 [] BreakCompile
            , ExtraDeps 2 [1] Irrelevant
            , StdDeps BreakTest
            , StdDeps Irrelevant
            , StdDeps FixCompile
            ],
            Blame [1..5],
            Blame [3])
      )
    , ("Complex dependencies",
           ([ StdDeps CreateTest
            , ExtraDeps 1 []  BreakCompile
            , ExtraDeps 2 [1] Irrelevant
            , ExtraDeps 3 []  Irrelevant
            , ExtraDeps 4 [3] BreakTest
            , ExtraDeps 5 [2] Irrelevant
            , ExtraDeps 6 [2] FixCompile
            ],
            Blame [1..6],
            Blame [4])
      )
    , ("Joined blame sequence",
           ([ StdDeps CreateTest
            , ExtraDeps 1 []  BreakCompile
            , ExtraDeps 2 [1]  BreakTest
            , ExtraDeps 3 [2] FixCompile
            ],
            Blame [1,2,3],
            Blame [1,2,3])
      )
    ]


type ExpectedResult =
  ( [WithDeps Transition]
  -- result without shrinking
  , StrategyResultRaw [Int]
  -- result with shrinking
  , StrategyResultRaw [Int]
  )

expectedResult :: O.TestStrategy -> (String, ExpectedResult) -> Test
expectedResult testStrategy (testName, (testDetails, expectedNoShrinkingResult, expectedShrinkingResult)) =
  testCase testName $ do
    let
      noShrinkingResult = runStrategyOn testStrategy O.NoShrinkFailure testDetails
      shrinkingResult = runStrategyOn testStrategy O.ShrinkFailure testDetails
    assertEqual "Unexpected result without shrinking" expectedNoShrinkingResult noShrinkingResult
    assertEqual "Unexpected result with shrinking" expectedShrinkingResult shrinkingResult


runStrategyOn :: O.TestStrategy -> O.ShrinkFailure -> [WithDeps Transition] -> StrategyResultRaw [Int]
runStrategyOn testStrategy shrinkFailure transitions =
  let initialState = TS CompileWorking TestNonExistent
      finalState = foldl (flip applyTransition) initialState (map withDepsContents transitions) in
  unseal (fmap toPatchNums . fst . flip runTestingMonad finalState . runStrategy testStrategy shrinkFailure)
         (genPatchSequence initialState transitions)

toPatchNums :: Sealed2 (PatchSeq Patch) -> [Int]
toPatchNums (Sealed2 ps) = mapFL (\(Patch n _ _) -> n) (patchTreeToFL ps)

genPatchSequence :: TestingState -> [WithDeps Transition] -> Sealed (RL Patch Origin)
genPatchSequence initialState transitions =
  mapSeal reverseFL $ unFreeLeft $ doGen 0 initialState transitions
  where
    doGen :: Int -> TestingState -> [WithDeps Transition] -> FreeLeft (FL Patch)
    doGen _ _ [] = emptyGap NilFL
    doGen n startingState (t:ts) =
      consGapFL (patch n t)
         (doGen (n+1) (applyTransition (withDepsContents t) startingState) ts)


data Transition =
    CreateTest | RemoveTest
  | BreakTest | FixTest
  | BreakCompile | FixCompile
  | Irrelevant
  deriving Show

type Deps = (Maybe Int, [Int])

data WithDeps a = StdDeps a | ExtraDeps Int [Int] a

withDepsContents :: WithDeps a -> a
withDepsContents (StdDeps v) = v
withDepsContents (ExtraDeps _ _ v) = v

data TestStatus = TestNonExistent | TestWorking | TestBroken
  deriving Show

data CompileStatus = CompileWorking | CompileBroken
  deriving Show

data TestingState =
  TS
  { tsCompile :: CompileStatus
  , tsTest :: TestStatus
  }
  deriving Show


invertTransition :: Transition -> Transition
invertTransition CreateTest   = RemoveTest
invertTransition RemoveTest   = CreateTest
invertTransition BreakTest    = FixTest
invertTransition FixTest      = BreakTest
invertTransition BreakCompile = FixCompile
invertTransition FixCompile   = BreakCompile
invertTransition Irrelevant   = Irrelevant

commutableTransition :: (Transition, Transition) -> Bool
commutableTransition (Irrelevant, _) = True
commutableTransition (_, Irrelevant) = True
commutableTransition (t1, t2) = (forTest t1 && forCompile t2) || (forCompile t1 && forTest t2)
  where
    forTest CreateTest = True
    forTest RemoveTest = True
    forTest BreakTest  = True
    forTest FixTest    = True
    forTest _          = False
    forCompile BreakCompile = True
    forCompile FixCompile   = True
    forCompile _ = False

applyTransition :: Transition -> TestingState -> TestingState
applyTransition Irrelevant  s = s
applyTransition CreateTest s@TS { tsTest = TestNonExistent } = s { tsTest = TestWorking     }
applyTransition RemoveTest s@TS { tsTest = TestWorking     } = s { tsTest = TestNonExistent }
applyTransition BreakTest  s@TS { tsTest = TestWorking     } = s { tsTest = TestBroken      }
applyTransition FixTest    s@TS { tsTest = TestBroken      } = s { tsTest = TestWorking     }
applyTransition BreakCompile s@TS { tsCompile = CompileWorking } = s { tsCompile = CompileBroken  }
applyTransition FixCompile   s@TS { tsCompile = CompileBroken  } = s { tsCompile = CompileWorking }
applyTransition transition state =
  error $ "Invalid transition " ++ show transition ++ " applied to state " ++ show state

data Patch wX wY = Patch Int Deps Transition

patch :: Int -> WithDeps Transition -> Patch wX wY
patch n (StdDeps t) = Patch n (Nothing, []) t
patch n (ExtraDeps name deps t) = Patch n (Just name, deps) t

instance Invert Patch where
  invert (Patch n deps t) = Patch n deps (invertTransition t)

instance Commute Patch where
  commute (Patch n1 d1@(name1, _) t1 :> Patch n2 d2@(_, deps2) t2)
    | name1 `elem` map Just deps2 = Nothing
    | commutableTransition (t1, t2) = Just (Patch n2 d2 t2 :> Patch n1 d1 t1)
    | otherwise = Nothing

toTestResult :: TestingState -> TestResult wX
toTestResult (TS { tsCompile = CompileBroken }                           ) = Untestable
toTestResult (TS { tsCompile = CompileWorking, tsTest = TestNonExistent }) = Untestable
toTestResult (TS { tsCompile = CompileWorking, tsTest = TestWorking     }) = Testable Success
toTestResult (TS { tsCompile = CompileWorking, tsTest = TestBroken      }) = Testable (Failure (TestFailure 1))

data TestingMonad wX wY a = TestingMonad { runTestingMonad :: TestingState -> (a, TestingState) }

instance Indexed.Monad TestingMonad where
  return v = TestingMonad (\n -> (v, n))
  m >>= f = TestingMonad (\n1 -> let (a, n2) = runTestingMonad m n1 in runTestingMonad (f a) n2)
  m1 >> m2 = TestingMonad (\n1 -> let (_, n2) = runTestingMonad m1 n1 in runTestingMonad m2 n2)

instance IndexedApply Patch where
  type ApplyState Patch = TestingMonad

  apply (Patch _ _ transition) =
    TestingMonad $ \s -> ((), applyTransition transition s)

  unapply (Patch _ _ transition) =
    TestingMonad $ \s -> ((), applyTransition (invertTransition transition) s)

instance TestRunner TestingMonad where
  type ApplyPatchReqs TestingMonad p = (IndexedApply p, ApplyState p ~ TestingMonad)
  type DisplayPatchReqs TestingMonad p = p ~ Patch

  writeMsg _ = Indexed.return ()
  mentionPatch _ = Indexed.return ()
  finishedTesting v = TestingMonad (\_ -> (v, error "something tried to read final testing state"))

  getCurrentTestResult = TestingMonad (\n -> (toTestResult n, n))

  applyPatch = apply
  unapplyPatch = unapply
