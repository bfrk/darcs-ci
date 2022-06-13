module Darcs.Repository.Flags
    ( Compression (..)
    , RemoteDarcs (..)
    , remoteDarcs
    , Reorder (..)
    , Verbosity (..)
    , UpdatePending (..)
    , UseCache (..)
    , DryRun (..)
    , UMask (..)
    , LookForAdds (..)
    , LookForReplaces (..)
    , DiffAlgorithm (..)
    , LookForMoves (..)
    , DiffOpts (..)
    , RunTest (..)
    , SetScriptsExecutable (..)
    , LeaveTestDir (..)
    , RemoteRepos (..)
    , SetDefault (..)
    , InheritDefault (..)
    , UseIndex (..)
    , CloneKind (..)
    , AllowConflicts (..)
    , ExternalMerge (..)
    , WorkRepo (..)
    , WantGuiPause (..)
    , WithPatchIndex (..)
    , WithWorkingDir (..)
    , ForgetParent (..)
    , PatchFormat (..)
    , WithPrefsTemplates (..)
    ) where

import Darcs.Prelude

import Darcs.Util.Cache ( Compression(..) )
import Darcs.Util.Diff ( DiffAlgorithm(..) )
import Darcs.Util.Global ( defaultRemoteDarcsCmd )


data Verbosity = Quiet | NormalVerbosity | Verbose
    deriving ( Eq, Show )

data WithPatchIndex = YesPatchIndex | NoPatchIndex
    deriving ( Eq, Show )

data RemoteDarcs = RemoteDarcs String
                 | DefaultRemoteDarcs
    deriving ( Eq, Show )

remoteDarcs :: RemoteDarcs -> String
remoteDarcs DefaultRemoteDarcs = defaultRemoteDarcsCmd
remoteDarcs (RemoteDarcs x) = x

data Reorder = NoReorder | Reorder
    deriving ( Eq )

data UpdatePending = YesUpdatePending | NoUpdatePending
    deriving ( Eq, Show )

data UseCache = YesUseCache | NoUseCache
    deriving ( Eq, Show )

data DryRun = YesDryRun | NoDryRun
    deriving ( Eq, Show )

data UMask = YesUMask String | NoUMask
    deriving ( Eq, Show )

data LookForAdds = NoLookForAdds | YesLookForAdds | EvenLookForBoring
    deriving ( Eq, Show )

data LookForReplaces = YesLookForReplaces | NoLookForReplaces
    deriving ( Eq, Show )

data LookForMoves = YesLookForMoves | NoLookForMoves
    deriving ( Eq, Show )

data DiffOpts = DiffOpts
  { withIndex :: UseIndex
  , lookForAdds :: LookForAdds
  , lookForReplaces :: LookForReplaces
  , lookForMoves :: LookForMoves
  , diffAlg :: DiffAlgorithm
  } deriving Show

data RunTest = YesRunTest | NoRunTest
    deriving ( Eq, Show )

data SetScriptsExecutable = YesSetScriptsExecutable | NoSetScriptsExecutable
    deriving ( Eq, Show )

data LeaveTestDir = YesLeaveTestDir | NoLeaveTestDir
    deriving ( Eq, Show )

data RemoteRepos = RemoteRepos [String]
    deriving ( Eq, Show )

data SetDefault = YesSetDefault Bool | NoSetDefault Bool
    deriving ( Eq, Show )

data InheritDefault = YesInheritDefault | NoInheritDefault
    deriving ( Eq, Show )

data UseIndex = UseIndex | IgnoreIndex deriving ( Eq, Show )

-- Various kinds of getting repositories
data CloneKind = LazyClone       -- ^Just copy pristine and inventories
               | NormalClone     -- ^First do a lazy clone then copy everything
               | CompleteClone   -- ^Same as Normal but omit telling user they can interrumpt
    deriving ( Eq, Show )

data AllowConflicts = NoAllowConflicts | YesAllowConflicts | YesAllowConflictsAndMark
    deriving ( Eq, Show )

data ExternalMerge = YesExternalMerge String | NoExternalMerge
    deriving ( Eq, Show )

data WorkRepo = WorkRepoDir String | WorkRepoPossibleURL String | WorkRepoCurrentDir
    deriving ( Eq, Show )

data WantGuiPause = YesWantGuiPause | NoWantGuiPause
    deriving ( Eq, Show )

data WithWorkingDir = WithWorkingDir | NoWorkingDir
    deriving ( Eq, Show )

data ForgetParent = YesForgetParent | NoForgetParent
    deriving ( Eq, Show )

data PatchFormat = PatchFormat1 | PatchFormat2 | PatchFormat3
    deriving ( Eq, Show )

data WithPrefsTemplates =  WithPrefsTemplates | NoPrefsTemplates
    deriving ( Eq, Show )
