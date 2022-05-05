{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Darcs.UI.Commands.Test.Impl
    ( TestRunner(..), runStrategy
    , TestResult(..), TestResultValid(..), TestFailure(..)
    , TestingDone
    , PatchTree(..), patchTreeToFL
    , StrategyResult, StrategyResultRaw(..)
    , explanatoryTextFor
    , runTestingEnv
    , exitCodeToTestResult
    , mkTestCmd
    , runTestable
    ) where

import Darcs.Prelude hiding ( init, Monad(..) )
import Darcs.Util.IndexedMonad

import qualified Control.Monad as Base ( Monad(..) )

import Data.Constraint ( Dict(..) )
import Data.String ( fromString )

import GHC.Exts ( Constraint )
import GHC.Show ( showSpace )

import System.Exit ( ExitCode(..) )
import System.IO ( hFlush, stdout )

import qualified Darcs.UI.Options.All as O
import Darcs.Repository ( setScriptsExecutablePatches )
import Darcs.Patch.Witnesses.Ordered
    ( RL(..)
    , FL(..)
    , (:>)(..)
    , splitAtRL
    , reverseRL
    , lengthRL
    , mapRL_RL
    , lengthFL
    , reverseFL
    , (+>+)
    )
import Darcs.Patch.Witnesses.Sealed ( Sealed2(..) )
import Darcs.Patch.Witnesses.Show
  ( Show1(..), Show2(..)
  , showsPrec2
  )
import Darcs.Patch.ApplyMonad ( ApplyMonad )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute, commute )
import Darcs.Patch.CommuteFn ( commuterIdFL )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch ( description )
import Darcs.Patch.Show ( ShowPatch )
import Darcs.Util.Printer ( putDocLn )
import Darcs.Repository.ApplyPatches ( DefaultIO, runDefault )

-- |This type is used to track the state of the testing tree.
-- For example, 'Testing IO wX wY Int' requires that the testing
-- tree start in state 'wX', and leaves it in state 'wY'.
newtype Testing m (wX :: *) (wY :: *) a = Testing { unTesting :: m a }

-- |Once we've finished tracking down a test failure, we no longer care
-- about tracking the actual state of the testing tree. This witness
-- constant is never used in any patch, so once we use it for the state
-- of the testing tree, in practice we can no longer do anything more with
-- that tree.
--
-- We could also use some kind of existential or different monad type
-- to represent this, but it would make composing code with 'do' harder.
data TestingDone

type TestingIO = Testing IO

instance Base.Monad m => Monad (Testing m) where
  return v = Testing (Base.return v)
  Testing m >>= f = Testing (m Base.>>= unTesting . f)
  Testing m1 >> Testing m2 = Testing (m1 Base.>> m2)

instance LiftIx Testing where
  liftIx = Testing

data TestingParams =
  TestingParams
  { tpSetScriptsExecutable :: O.SetScriptsExecutable
  , tpTestCmd :: TestCmd
  }

-- |The 'Testing' monad, augmented with configuration parameters
newtype TestingEnv m wX wY a =
  TestingEnv { unTestingEnv :: ReaderT TestingParams (Testing m) wX wY a }

type TestingEnvIO = TestingEnv IO

deriving instance Base.Monad m => Monad (TestingEnv m)
deriving instance Base.Monad m => MonadReader TestingParams (TestingEnv m)

instance LiftIx TestingEnv where
  liftIx m = TestingEnv (ReaderT (\_ -> liftIx m))

runTestingEnv :: TestingParams -> TestingEnv m wA TestingDone a -> m a
runTestingEnv args = unTesting . ($ args) . runReaderT . unTestingEnv

liftTesting :: Testing m wX wY a -> TestingEnv m wX wY a
liftTesting m = TestingEnv $ ReaderT $ \_ -> m

-- |An indexed monad that can be used to run tests. 'TestingEnvIO' is
-- the only real implementation, the unit tests for testing are based on
-- mock implementations.
class Monad m => TestRunner m where
  type ApplyPatchReqs m (p :: * -> * -> *) :: Constraint
  type DisplayPatchReqs m (p :: * -> * -> *) :: Constraint

  -- |Output a message
  writeMsg :: String -> m wX wX ()

  -- |Output a message containing the name of a patch
  mentionPatch :: DisplayPatchReqs m p => p wA wB -> m wX wX ()
  
  -- |Apply a patch to the testing tree.
  applyPatch :: ApplyPatchReqs m p => p wX wY -> m wX wY ()

  -- |Unapply a patch from the testing tree
  unapplyPatch :: ApplyPatchReqs m p => p wX wY -> m wY wX ()

  -- |Get the current status (pass/skip/fail) of the testing tree,
  -- e.g. by running the test command.
  getCurrentTestResult :: m wX wX (TestResult wX)

  -- |Flag that all testing has completed.
  finishedTesting :: a -> m wX TestingDone a

type TestRunnerPatchReqs m p =
  ( -- Having to enumerate these different cases for ApplyPatchReqs is
    -- a bit ugly, but necessary because it is a type function and we
    -- don't know that ApplyPatchReqs m p => ApplyPatchReqs m (FL p), etc.
    -- In theory QuantifiedConstraints could be used to simplify this but
    -- the fact that ApplyPatchReqs is a type function makes this a bit tricky.
    ApplyPatchReqs m p, ApplyPatchReqs m (RL p), ApplyPatchReqs m (FL p)
  , ApplyPatchReqs m (PatchTree p), ApplyPatchReqs m (RL (PatchTree p))
  , DisplayPatchReqs m p)

type TestablePatch m p = (TestRunner m, TestRunnerPatchReqs m p, Commute p)

instance TestRunner TestingEnvIO where
  type ApplyPatchReqs TestingEnvIO p = (Apply p, ApplyMonad (ApplyState p) DefaultIO, PatchInspect p)
  type DisplayPatchReqs TestingEnvIO p = ShowPatch p

  writeMsg str = liftIx (putStrLn str Base.>> hFlush stdout)
  mentionPatch p = liftIx (putDocLn (description p) Base.>> hFlush stdout)

  applyPatch p = do
    liftTesting $ Testing $ runDefault (apply p)
    opts <- asks tpSetScriptsExecutable
    when (opts == O.YesSetScriptsExecutable) $
      liftIx $ setScriptsExecutablePatches p

  unapplyPatch p = do
    liftTesting $ Testing $ runDefault (unapply p)
    opts <- asks tpSetScriptsExecutable
    when (opts == O.YesSetScriptsExecutable) $
      liftIx $ setScriptsExecutablePatches p

  getCurrentTestResult = do
    testCmd <- asks tpTestCmd
    liftTesting $ runTestCmd testCmd

  finishedTesting r = TestingEnv $ ReaderT $ \_ -> Testing (Base.return r)

-- |The result of running a test on state 'wX' of the repository.
data TestResult wX
  = Testable (TestResultValid wX) -- ^We got a usable test result.
  | Untestable
    -- ^The test result could not be identified as either pass or fail,
    -- for example it might have been a build failure. External test
    -- scripts report this by reporting exit code 125.

-- |A usable test result, i.e. not an untestable state.
data TestResultValid wX
  = Success -- ^The test passed.
  | Failure (TestFailure wX) -- ^The test failed with the given exit code.

data TestFailure wX = TestFailure Int

exitCodeToTestResult :: ExitCode -> TestResult wX
exitCodeToTestResult ExitSuccess = Testable Success
exitCodeToTestResult (ExitFailure 125) = Untestable
exitCodeToTestResult (ExitFailure n) = Testable (Failure (TestFailure n))

-- |A 'TestCmd' runs the test on a given repository state.
data TestCmd = TestCmd (forall (wX :: *) . TestingIO wX wX (TestResult wX))

runTestCmd :: TestCmd -> TestingIO wX wX (TestResult wX)
runTestCmd (TestCmd cmd) = cmd

mkTestCmd :: (forall (wX :: *) . IO (TestResult wX)) -> TestCmd
mkTestCmd cmd = TestCmd (Testing cmd)

-- |'PatchTree' is a sequence of patches, balanced in an arbitrary
-- way depending on how it happened to be constructed.
-- In the 'darcs test' implementation it is used to
-- wrap up a single patch or group of patches that might be the cause of a failure.
data PatchTree p wX wY where
  Single :: p wX wY -> PatchTree p wX wY
  Joined :: PatchTree p wX wY -> PatchTree p wY wZ -> PatchTree p wX wZ

instance Show2 p => Show (PatchTree p wX wY) where
  showsPrec prec (Single p) =
    showParen (prec >= 11) (showString "Darcs.UI.Commands.Test.Single " . showsPrec2 11 p)
  showsPrec prec (Joined p1 p2) =
    showParen (prec >= 11)
              (showString "Darcs.UI.Commands.Test.Joined "
                 . showsPrec2 11 p1 . showSpace . showsPrec2 11 p2)


instance Show2 p => Show1 (PatchTree p wX) where
  showDict1 = Dict

instance Show2 p => Show2 (PatchTree p) where
  showDict2 = Dict

instance Apply p => Apply (PatchTree p) where
  type ApplyState (PatchTree p) = ApplyState p
  apply (Single p) = apply p
  apply (Joined p1 p2) = apply p1 Base.>> apply p2
  unapply (Single p) = unapply p
  unapply (Joined p1 p2) = unapply p2 Base.>> unapply p1

instance PatchInspect p => PatchInspect (PatchTree p) where
  listTouchedFiles (Single p) = listTouchedFiles p
  listTouchedFiles (Joined p1 p2) = listTouchedFiles p1 ++ listTouchedFiles p2
  hunkMatches f (Single p) = hunkMatches f p
  hunkMatches f (Joined p1 p2) = hunkMatches f p1 || hunkMatches f p2

patchTreeToFL :: PatchTree p wX wY -> FL p wX wY
patchTreeToFL t = go t NilFL
  where
    go :: PatchTree p wA wB -> FL p wB wC -> FL p wA wC
    go (Single p) rest = p :>: rest
    go (Joined p1 p2) rest = go p1 (go p2 rest)

flToPatchTree :: p wX wY -> FL p wY wZ -> PatchTree p wX wZ
flToPatchTree p NilFL = Single p
flToPatchTree p (q :>: qs) = Joined (Single p) (flToPatchTree q qs)

rlToPatchTree :: RL p wX wY -> p wY wZ -> PatchTree p wX wZ
rlToPatchTree NilRL p = Single p
rlToPatchTree (qs :<: q) p = Joined (rlToPatchTree qs q) (Single p)

-- |The result of running a test strategy.
data StrategyResultRaw patches =
    NoPasses -- ^The chosen strategy didn't find any passing states in the repository.
  | NoFailureOnHead -- ^The test didn't fail on head so there's no failure to track down.
  | Blame patches -- ^The failure was tracked down to the given patches.
  -- these two are just for oneTest
  | RunSuccess -- ^The single test run passed.
  | RunFailed Int -- ^The single test run failed with the given exit code.
  deriving (Eq, Show, Functor)

type StrategyResult p wSuccess wFailure =
  StrategyResultRaw (PatchTree p wSuccess wFailure)

type StrategyResultSealed p =
  StrategyResultRaw (Sealed2 (PatchTree p))

-- |'WithResult' is a continuation passed to a test strategy indicating
-- what should be done with the final result of the strategy. This for
-- example allows a post-processing "minimise blame" pass to be run.
-- The witnesses make it hard to wrap this up in a standard abstraction.
data WithResult (m :: * -> * -> * -> *) p a =
  WithResult
  { runWithResult
      :: forall wSuccess wFailure
       . StrategyResult p wSuccess wFailure
      -> m wSuccess TestingDone a
  }

-- |After a strategy has finished, untestable states might mean that it
-- was only able to assign blame to a group of patches rather than a
-- single patch. This function tries to reorder the group of patches
-- (using commutation). The hope is that a reordered sequence will reveal
-- a testable state, allowing us to cut down the group.
--
-- The type is logically
-- something like 'StrategyResult -> m StrategyResult', but is expressed
-- as a transformation of a 'WithResult' to manage the witnesses. These
-- are complicated because we want to re-use the testing tree left by the
-- strategy.
minimiseBlame :: forall m p a . TestablePatch m p => WithResult m p a -> WithResult m p a
minimiseBlame (WithResult finalRunner) =
  WithResult $ \result ->
    case result of
      Blame p -> doMinimiseFwd NilRL (patchTreeToFL p)
      _ -> finalRunner result
  where
    -- This minimisation code is a bit ad-hoc and almost certainly
    -- doesn't find every possible minimisation (which might require
    -- an exponential search). It also doesn't cache anything and
    -- therefore may do some repeated shuffling.
    
    -- The witnesses do guarantee that it is
    -- correct and the implementation is structured to guarantee
    -- termination.

    -- The overall algorithm is to work through the sequence from left
    -- to right, treating each patch in turn as a 'focus'. We then try
    -- to commute the focus with the patches to the left of it, and test
    -- each new intermediate state this produces.
    --
    -- If we do find a testable intermediate state, we can chop the sequence
    -- at that state.

    -- In 'doMinimiseFwd kept rest', 'kept' are the patches that we
    -- have looked at already, and 'rest' are the ones still to be
    -- processed.
    doMinimiseFwd
      :: RL p wSuccess wFocus
      -> FL p wFocus wFailure
      -> m wFocus TestingDone a

    doMinimiseFwd kept (focus :>: rest) = do
      -- Call 'doMinimiseRev' to work on the first of the so-far-unprocessed
      -- patches. In the end 'doMinimiseRev' will call back to 'doMinimiseFwd',
      -- and either 'focus' will have been moved into 'kept' or dropped entirely
      -- because the sequence has been cut down.
      --
      -- Whilst 'kept' marks the patches that have already been visited,
      -- 'doMinimiseRev' will still try to commute them with the 'focus' patch.
      doMinimiseRev kept (focus :>: NilFL :> NilFL :> rest)

    doMinimiseFwd (kept :<: final) NilFL = do
      -- This unapply is only needed because WithResult
      -- is based around leaving the test tree in the 'wSuccess'
      -- state in case something else needs it.
      -- In practice no more tests will be run after we finish minimising blame,
      -- so it's wasted work.
      -- It could probably be removed by making the type of WithResult
      -- more sophisticated somehow, but it's not clear the complexity
      -- is worth it.
      unapplyPatch (kept :<: final)
      finalRunner (Blame (rlToPatchTree kept final))

    doMinimiseFwd NilRL NilFL = error "internal error: trying to minimise an empty sequence"

    -- In 'doMinimiseRev tocommute (focus :> ps :> qs)':
    --   - 'qs' are the patches that are yet to be processed. They will just be sent
    --     back to 'doMinimiseFwd' unless we end up dropping them entirely.
    --   - 'ps' are patches we have managed to commute with 'focus' but still produced
    --     untestable states.
    --   - 'focus' are the patches we are trying to move around to see if it helps
    --     find a testable state. It starts out as a singleton but gains more patches
    --     as commutes fail.
    --   - 'tocommute' are the patches we still need to commute with the 'focus'.
    doMinimiseRev
      :: RL p wSuccess wFocus
      -> (FL p :> FL p :> FL p) wFocus wFailure
      -> m wFocus TestingDone a

    doMinimiseRev NilRL (focus :> ps :> qs) = do
      -- We've run out of things to commute, so pass everything that we
      -- looked at back to 'doMinimiseFwd' as the 'kept' parameter.
      let kept = reverseFL (focus +>+ ps)
      applyPatch kept
      doMinimiseFwd kept qs

    doMinimiseRev (tocommute :<: p) (focus :> ps :> qs) = do
      unapplyPatch p
      case commuterIdFL commute (p :> focus) of
        Nothing ->
          -- if we can't commute just attach it to the focus
          doMinimiseRev tocommute (p :>: focus :> ps :> qs)
        Just (focus' :> p') -> do
          applyPatch focus'
          testResult <- getCurrentTestResult
          case testResult of
            Untestable -> do
              -- The newly commuted state is also untestable, leave the patch we
              -- just commuted in 'ps' and keep working on the focus.
              unapplyPatch focus'
              doMinimiseRev tocommute (focus' :> p' :>: ps :> qs)
            -- Since we got a result, we can chop the sequence here, we just need
            -- to decide which part to keep.
            -- The full sequence after the commute is kept ; focus' | p' ; ps ; qs
            Testable Success -> doMinimiseRev NilRL (NilFL :> p' :>: ps :> qs)
            Testable (Failure _) -> do
              unapplyPatch focus'
              doMinimiseRev tocommute (focus' :> NilFL :> NilFL)

-- |StrategyDone captures the final result of running a "test strategy" like
-- bisect, backoff, linear or once. It has a slightly complicated type because of the
-- witnesses and because we may want to run a continuation afterwards to minimise
-- the result. Essentially it is just a 'StrategyResult'.
type StrategyDone m p wY = forall a . WithResult m p a ->  m wY TestingDone a

-- |Report that the strategy has finished with the given result.
strategyDone :: TestRunner m => StrategyResult p wSuccess wFailure -> StrategyDone m p wSuccess
strategyDone result withResult = runWithResult withResult result

-- |The implementation type for a given "test strategy" like bisect, backoff, linear or once.
-- It is given a sequence of patches we might want to search inside to identify the cause of
-- a test failure, and also passed the initial testing result for the end of that sequence.
type Strategy
   = forall m p wOlder wNewer
   . TestablePatch m p
  => TestResult wNewer
  -> RL p wOlder wNewer
  -> StrategyDone m p wNewer

-- runStrategy orchestrates the whole process of isolating patches
-- triggering the failure.
runStrategy
  :: TestablePatch m p
  => O.TestStrategy
  -> O.ShrinkFailure
  -> RL p wOlder wNewer
  -> m wNewer TestingDone (StrategyResultSealed p)
runStrategy strategy shrinkFailure patches = do
  -- The starting point is a full patch sequence 'RL p wStart wEnd' with the
  -- testing tree in state 'wEnd'. We get the initial testing result for that
  -- state as 'Strategy' requires it.
  testResult <- getCurrentTestResult
  -- We narrow down the failure via a strategy (linear/bisect/backoff). If we
  -- find patches to blame, this has type 'Testing p wSuccess wFailure', leaving the testing
  -- tree in state 'wSuccess'.
  -- If the strategy is "one test" then the result is just success/failure.
  chooseStrategy strategy testResult patches $
    -- What to do with the result of the strategy is passed as a continuation to the strategy.
    -- First we try to minimise any patches to blame, resulting in 'Testing p wSuccess2 wFailure2'.
    -- The testing tree is left in state 'wSuccess2' although we don't actually care about
    -- it any more.
    (if shrinkFailure == O.ShrinkFailure then minimiseBlame else id) $
    -- Finally the result is wrapped up in a Sealed2 and returned.
    WithResult (finishedTesting . fmap Sealed2)

runTestable
  :: ( Commute p
     , TestRunner (TestingEnv m)
     , TestRunnerPatchReqs (TestingEnv m) p
     )
  => O.SetScriptsExecutable
  -> TestCmd
  -> O.TestStrategy
  -> O.ShrinkFailure
  -> RL p wStart wA
  -> m (StrategyResultSealed p)
runTestable sse tcmd strategy shrinkFailure ps =
  runTestingEnv (TestingParams sse tcmd) $ runStrategy strategy shrinkFailure ps

chooseStrategy :: O.TestStrategy -> Strategy
chooseStrategy O.Bisect = trackBisect
chooseStrategy O.Linear = trackLinear
chooseStrategy O.Backoff = trackBackoff
chooseStrategy O.Once = oneTest

explanatoryTextFor :: O.TestStrategy -> String
explanatoryTextFor strategy =
  case strategy of
    O.Bisect -> assumedMonotony
    O.Backoff -> assumedMonotony
    O.Linear -> wasLinear
    O.Once -> wasLinear -- this case won't actually be reached
  where
    -- We did a bisection type search so a given patch that causes
    -- the failure is only the most recent if there is actually only
    -- one transition from "passed" to "failed" in the repository history.
    assumedMonotony = " (assuming monotony in the given range)"
    -- We did a linear search so the patch we found is definitely the
    -- most recent to have triggered a failure.
    wasLinear = ""

-- | test only the last recorded state
oneTest :: Strategy
oneTest (Testable Success) _ = strategyDone RunSuccess
oneTest Untestable  _ = strategyDone $ RunFailed 125
oneTest (Testable (Failure (TestFailure n)))  _ = strategyDone $ RunFailed n

-- | linear search (with --linear)
trackLinear :: Strategy
trackLinear (Testable (Failure _)) ps = trackNextLinear NilFL ps
trackLinear _ _ = strategyDone NoFailureOnHead

-- |The guts of tracking down a test failure by linear search
-- Precondition: 'wZ' is a failing state and any states
-- in the (possibly empty) range of states '[wY, wZ)' are untestable.
trackNextLinear
  :: TestablePatch m p
  => FL p wY wZ -- ^a buffer of patches that start with an untestable state
  -> RL p wX wY -- ^patches we haven't visited yet
  -> StrategyDone m p wY
trackNextLinear _ NilRL withResult = strategyDone NoPasses withResult
trackNextLinear untestables (ps:<:p) withResult = do
  unapplyPatch p
  writeMsg "Trying without the patch:"
  mentionPatch p
  testResult <- getCurrentTestResult
  case testResult of
    -- If the test passes we're done.
    Testable Success -> strategyDone (Blame (flToPatchTree p untestables)) withResult
    -- If the test fails then we can drop the 'untestables' buffer and keep going.
    Testable (Failure _) -> trackNextLinear NilFL ps withResult
    -- If the state is untestable then we add to the 'untestables' buffer and keep going.
    Untestable -> trackNextLinear (p :>: untestables) ps withResult

-- |A 'TestingState' is used to keep track of the set of patches
-- a search strategy is currently working on, split at a given point
-- with an explicit witness for that intermediate point (the 'focus'),
-- so we can connect it to the state of the testing tree.
data TestingState p wOlder wFocus wNewer where
  TestingState
    :: RL (PatchTree p) wOlder wFocus
    -> FL (PatchTree p) wFocus wNewer
    -> TestingState p wOlder wFocus wNewer

lengthTS :: TestingState p wX wZ wY -> Int
lengthTS (TestingState ps1 ps2) = lengthRL ps1 + lengthFL ps2

lengthsTS :: TestingState p wX wZ wY -> (Int, Int)
lengthsTS (TestingState ps1 ps2) = (lengthFL ps2, lengthRL ps1)

-- |Exponential backoff search (with --backoff): first search backwards looking for
-- a successful state, then bisect between that successful state and the current (failed)
-- state.
trackBackoff :: Strategy
trackBackoff (Testable (Failure tf)) ps =
  -- 4 is an arbitrary choice for how far to start jumping backwards
  trackNextBackoff tf 4 (mapRL_RL Single ps)
trackBackoff _ _ = strategyDone NoFailureOnHead

-- |Precondition: the test fails at 'wNewer'.
trackNextBackoff
  :: TestablePatch m p
  => TestFailure wNewer -- ^Failure witness
  -> Int -- ^Number of patches to skip.
  -> RL (PatchTree p) wOlder wNewer -- ^Patches not yet skipped.
  -> StrategyDone m p wNewer

-- Normal base case: we've run out of patches.
trackNextBackoff _ _ NilRL withResult = strategyDone NoPasses withResult

-- Edge case: if there's just one patch left then either the test
-- passes before this patch and we can blame it, or we've run out of
-- places to look for success.
trackNextBackoff _ _ (NilRL :<: p) withResult = do
  unapplyPatch p
  testResult <- getCurrentTestResult
  case testResult of
    Testable Success -> strategyDone (Blame p) withResult
    _ -> strategyDone NoPasses withResult

-- There's more than one patch to go.
trackNextBackoff tf n ahead withResult = do
  case splitAtRL n ahead of
    ahead' :> skipped' -> do
      writeMsg $ "Skipping " ++ show n ++ " patches..."++show (lengthRL skipped', lengthRL ahead')
      unapplyPatch skipped'
      -- After backing off by n more patches, look for a testable state, working through the skipped
      -- patches if necessary because the current state isn't testable.
      findTestableTowardsNewer (Failure tf) (TestingState ahead' (reverseRL skipped')) $
        \testResult (TestingState ahead'' skipped'') ->
        case testResult of
          -- Another failure, keep going. Note that it's possible that
          -- findTestableTowardsNewer will have to go all the way to the end of
          -- skipped', leaving us in the same testing position as before, but
          -- the backoff count is doubled so we'll still make progress.
          Failure tf2 -> trackNextBackoff tf2 (2*n) ahead'' withResult
          -- Found a success state, so now we can start the bisect.
          Success -> initialBisect (TestingState NilRL skipped'') withResult

-- |Given a patch sequence which has a valid test result at the end ('wNewer'),
-- try to find another point with a valid test result, starting from 'wFocus' and
-- jumping towards 'wNewer' if necessary.
findTestableTowardsNewer
  :: TestablePatch m p
  => TestResultValid wNewer
  -> TestingState p wOlder wFocus wNewer
  -> (forall wFocus2
       . TestResultValid wFocus2
      -> TestingState p wOlder wFocus2 wNewer
      -> m wFocus2 wResult a
     )
  -> m wFocus wResult a

findTestableTowardsNewer newerResult ts@(TestingState _ NilFL) cont = cont newerResult ts
findTestableTowardsNewer newerResult ts@(TestingState older (p :>: ps)) cont = do
  focusResult <- getCurrentTestResult
  case focusResult of
    Testable res -> cont res ts
    Untestable -> do
      writeMsg $ "Found untestable state " ++ show (lengthsTS ts)
      applyPatch p
      let
        -- The 'wB' state is untestable, so try to attach the patches on either side of
        -- it together into the same 'PatchTree' so we don't try it again.
        joinT :: RL (PatchTree p) wA wB -> PatchTree p wB wC -> RL (PatchTree p) wA wC
        -- If we don't have any patches on the left, we can't do anything.
        joinT NilRL x = NilRL :<: x
        -- Otherwise peel off the first patch on the left and attach it to the patch on the right.
        joinT (ys :<: y) x = ys :<: Joined y x
      moveHalfNewer (TestingState (joinT older p) ps) $ \tsNew ->
        findTestableTowardsNewer newerResult tsNew cont


-- |Binary search (with --bisect): bisect from the start of the repository.
-- This strategy is a bit dubious as the test probably doesn't actually pass
-- at the start of the repository so the hope is that at some point during the
-- bisect we will come across a passing state. The two different entry points into
-- 'initialBisect' (trackBisect and trackBackoff) also complicate the set of cases
-- we have to consider.
trackBisect :: Strategy
trackBisect (Testable (Failure _)) ps = initialBisect (TestingState (mapRL_RL Single ps) NilFL)
trackBisect _ _ = strategyDone NoFailureOnHead

-- |Progress of Bisect: current step, currently predicted total steps.
-- The total steps prediction will increase if we run into untestable states.
type BisectProgress = (Int, Int)

-- |Launch a bisect. Precondition: the test fails at 'wNewer'.
-- If called via backoff, then the test also passes at 'wOlder',
-- but there is no guarantee if bisect is called directly. 
initialBisect
  :: TestablePatch m p
  => TestingState p wOlder wFocus wNewer
  -> StrategyDone m p wFocus
initialBisect ps = trackNextBisect currProg ps
  where
    flooredLength = lengthTS ps `min` 1
    maxProg  = 1 + round ((logBase 2 $ fromIntegral flooredLength) :: Double)
    currProg = (1, maxProg) :: BisectProgress

-- |Given a testing state, work out what to do next.
-- Precondition: the test fails at 'wNewer'.
trackNextBisect
  :: forall m p wOlder wNewer wFocus
   . TestablePatch m p
  => BisectProgress
  -> TestingState p wOlder wFocus wNewer
  -> StrategyDone m p wFocus

trackNextBisect _ (TestingState NilRL NilFL) withResult = strategyDone NoPasses withResult

-- With these two cases we're down to a single patch, so either it's to blame
-- or there are no passing states found (subject to the limitations of the bisect strategy -
-- not every state was visited).
trackNextBisect _ (TestingState NilRL (p :>: NilFL)) withResult = checkAndReturnFinalBisectResult p withResult
trackNextBisect _ (TestingState (NilRL :<: p) NilFL) withResult = do
  unapplyPatch p
  checkAndReturnFinalBisectResult p withResult

-- More than one patch left. Find the middle of the TestingState and work from that.
trackNextBisect (dnow, dtotal) ps withResult = do
  writeMsg $ "Trying " ++ show dnow ++ "/" ++ show dtotal ++ " sequences..." ++ show (lengthsTS ps)
  moveToMiddle ps (\ts -> runNextBisect (dnow, dtotal) ts withResult)

-- |Once we only have one patch left in bisect, we need to check that the test passes before the patch.
-- This is not guaranteed when bisect is called directly from the command-line. If we changed the UI to
-- ensure that bisect was only launched with both a passing and a failing state, we could strengthen
-- the precondition of 'initialBisect' and things it calls, and this function would be unnecessary.
-- Precondition: the test fails at 'wNewer'.
checkAndReturnFinalBisectResult
  :: TestablePatch m p
  => PatchTree p wOlder wNewer
  -> StrategyDone m p wOlder
checkAndReturnFinalBisectResult p withResult = do
  testResult <- getCurrentTestResult
  case testResult of
    Testable Success -> strategyDone (Blame p) withResult
    _ -> strategyDone NoPasses withResult

-- |The guts of bisection. Normally it will be passed an evenly split
-- 'TestingState older newer' with the focus in the middle, but if we find an
-- untestable state then we will start jumping around to find something testable.
-- Preconditions: 'older' is non-empty; the test fails at wNewer.
runNextBisect
  :: forall m p wOlder wNewer wFocus
   . TestablePatch m p
  => BisectProgress
  -> TestingState p wOlder wFocus wNewer
  -> StrategyDone m p wFocus
runNextBisect (dnow, dtotal) (TestingState older newer) withResult = do
  testResult <- getCurrentTestResult
  case testResult of

    -- The standard case for bisect: we have a result for the focus and we use it to pick
    -- either the left or right half.
    Testable result -> do
      let doNext newState = trackNextBisect (dnow+1, dtotal) newState withResult
      case result of
        Success   -> doNext (TestingState NilRL newer) -- continue left  (to the present)
        Failure _ -> doNext (TestingState older NilFL) -- continue right (to the past)

    -- If we couldn't test the bisect state then we need to move around to try to find
    -- a testable state.
    Untestable -> do
      writeMsg $ "Found untestable state " ++ show (lengthsTS (TestingState older newer))
      case (older, newer) of
        (NilRL, _) -> error "internal error: older bisect state reached 0 patches (runNextBisect)"
        -- Although 'newer' can become empty, the precondition that the test fails at wNewer means
        -- we shouldn't get here.
        -- TODO the user might supply an unreliable test script, maybe we should deal with the NilFL
        -- case before running the test.
        (_, NilFL) -> error "internal error: newer bisect state reached 0 patches (runNextBisect)"
        (older' :<: p1, p2 :>: newer') -> do
          applyPatch p2
          moveHalfNewer (TestingState (older' :<: Joined p1 p2) newer') $
            \ts -> runNextBisect (dnow+1, dtotal+1) ts withResult

-- |Given a 'TestingState older newer', move the focus to the middle of 'newer',
-- updating the testing tree to match, and call the given continuation.
moveHalfNewer
  :: forall m p wOlder wNewer wFocus wResult a
   . TestablePatch m p
  => TestingState p wOlder wFocus wNewer
  -> (forall wFocus2 . TestingState p wOlder wFocus2 wNewer -> m wFocus2 wResult a)
  -> m wFocus wResult a

moveHalfNewer (TestingState older newer) f = doMove older (lengthFL newer `div` 2, newer)
  where
    doMove
      :: forall wFocus2
       . RL (PatchTree p) wOlder wFocus2
      -> (Int, FL (PatchTree p) wFocus2 wNewer)
      -> m wFocus2 wResult a

    doMove ps1 (0, ps2) = f (TestingState ps1 ps2)
    doMove _ (_, NilFL) = error "impossible: exhausted newer patches (moveHalfNewer)"
    doMove ps1 (n, p :>: ps2) = do
      applyPatch p
      doMove (ps1 :<: p) (n-1, ps2)

-- |Given a 'TestingState older newer', move the focus to the middle of
-- 'older +>+ newer', updating the testing tree to match, and call the given
-- continuation.
moveToMiddle
  :: forall m p wOlder wNewer wFocus wResult a
   . TestablePatch m p
  => TestingState p wOlder wFocus wNewer
  -> (forall wFocus2 . TestingState p wOlder wFocus2 wNewer -> m wFocus2 wResult a)
  -> m wFocus wResult a

moveToMiddle (TestingState older newer) f = doMove (lengthRL older, older) (lengthFL newer, newer)
  where
    doMove
      :: forall wFocus2
       . (Int, RL (PatchTree p) wOlder wFocus2)
      -> (Int, FL (PatchTree p) wFocus2 wNewer)
      -> m wFocus2 wResult a

    doMove (len1, ps1) (len2, ps2) | abs (len1 - len2) <= 1 = f (TestingState ps1 ps2)

    doMove (len1, ps1 :<: p1) (len2, ps2) | len1 > len2 = do
      unapplyPatch p1
      doMove (len1-1, ps1) (len2+1, p1 :>: ps2)

    doMove (len1, ps1) (len2, p2 :>: ps2) = do -- len2 > len1
      applyPatch p2
      doMove (len1+1, ps1 :<: p2) (len2-1, ps2)

    -- these cases should only be reachable if the lengths get out of sync
    doMove (_, NilRL) _ = error "internal error: right bisect state reached 0 patches (moveToMiddle)"
    doMove _ (_, NilFL) = error "internal error: left bisect state reached 0 patches (moveToMiddle)"
