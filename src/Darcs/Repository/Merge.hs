-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
-- Copyright (C) 2009 Petr Rockai
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.


module Darcs.Repository.Merge
    ( tentativelyMergePatches
    , considerMergeToWorking
    ) where

import Darcs.Prelude

import Control.Monad ( when, unless )
import System.Exit ( exitSuccess )
import System.IO.Error
    ( catchIOError
    , ioeGetErrorType
    , isIllegalOperationErrorType
    )

import Darcs.Util.Tree( Tree )
import Darcs.Util.File ( backupByCopying )

import Darcs.Patch
    ( RepoPatch, PrimOf, merge
    , effect
    , listConflictedFiles )
import Darcs.Patch.Apply ( ApplyState )
import Darcs.Patch.Depends ( slightlyOptimizePatchset )
import Darcs.Patch.Invertible ( mkInvertible )
import Darcs.Patch.Named ( patchcontents, anonymous )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, hopefully )
import Darcs.Patch.Progress( progressFL, progressRL )
import Darcs.Patch.Set ( PatchSet, Origin, appendPSFL, patchSet2RL )
import Darcs.Patch.Witnesses.Ordered
    ( FL(..), RL(..), Fork(..), (:\/:)(..), (:/\:)(..), (+>+), (+<<+)
    , lengthFL, mapFL_FL, concatFL, reverseFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), seal )

import Darcs.Repository.Flags
    ( DiffOpts (..)
    , AllowConflicts (..)
    , ResolveConflicts (..)
    , Reorder (..)
    , UpdatePending (..)
    , WantGuiPause (..)
    )
import Darcs.Repository.Hashed
    ( tentativelyAddPatches_
    , tentativelyRemovePatches_
    , UpdatePristine(..)
    )
import Darcs.Repository.Pristine
    ( applyToTentativePristine
    )
import Darcs.Repository.InternalTypes ( AccessType(RW), Repository, repoLocation )
import Darcs.Repository.Pending ( setTentativePending )
import Darcs.Repository.Resolution
    ( StandardResolution(..)
    , announceConflicts
    , haveConflicts
    , externalResolution
    , patchsetConflictResolutions
    , standardResolution
    )
import Darcs.Repository.State ( unrecordedChanges, readUnrecorded )

import Darcs.Util.Prompt ( promptYorn )
import Darcs.Util.Path ( anchorPath, displayPath )
import Darcs.Util.Progress( debugMessage )
import Darcs.Util.Printer.Color ( ePutDocLn )
import Darcs.Util.Printer ( redText, vcat )

data MakeChanges = MakeChanges | DontMakeChanges deriving ( Eq )

{- 'tentativelyMergePatches' is not easy to understand by just staring at
the code. So here is an in-depth explanation.

We start out at the state X at which our repo and the their repo deviate,
assuming any patches common to both repos have first been commuted to the
common part before X. So X is the intermediate state that is existentially
hiddden inside the Fork we get passed as argument. R is our recorded state
and Y is the recorded state of their repo.

 Y       R
  \     /
 them  us
    \ /
     X
     |
   common
     |
     O

We will elide the common part from now on. It doesn't change and we only
pass it unmodified to standardResolution, see below.

The easy part is to merge the local patches (us) with the remote ones
(them), giving us them' and us'.

     T
    / \
  us'  them'
  /     \
 Y       R
  \     /
 them  us
    \ /
     X


We can ignore us' and just add them' on top of us (which are already in our
repo), unless --reorder-patches is in effect, in which case we remove us and
then first add them and afterwards us'. The new state on top is T which
stands for the new /tentative/ state i.e. what will become the recorded
state after we finalize our changes.

But we're not done yet: we must also adapt the pending patch and the working
tree. Note that changing the working tree is not done in this procedure, we
merely return a list of prims to apply to working. Let us add the difference
between pristine and working, which we call pw, to the picture.

     T       U
    / \     /
 us' them' pw
  /     \ /
 Y       R
  \     /
 them  us
    \ /
     X

It is easy to see now that we must merge pw with them', as both start at the
(old) recorded state. This gives us pw' and them''.

         U'
        / \
      pw' them''
      /     \
     T       U
    / \     /
 us' them' pw
  /     \ /
 Y       R
  \     /
 them  us
    \ /
     X

Since U is our unrecorded state, them'' leads us from our old unrecorded
state to the new one, so this is what we will return (if there are no
conflicts; if there are, see below).

What about the pending patch? It starts at R and goes half-way toward U
since it is a prefix of pw. The new pending should start at T and go
half-way toward the new working state U'. Instead of adapting the old
pending patch, we set the new pending patch to pw', ignoring the old one.
This relies on sifting to commute out and drop the parts that need not be in
the pending patch, which is done when we finalize the tentative changes.

Up to now we did not consider conflicts. Any new conflicts arising from the
merges we made so far must be "resolved", that is, marked for manual
resolution, if possible, or at least reported to the user. We made two
merges, one with us and one with pw. It is important now to realize that our
existing repo, and in particular the sequence us, could already be
conflicted. Our job is to resolve only /new/ conflicts and not any
unresolved conflicts that were already in our repo. So, from the rightmost
branch of our double merge us+>+pw+>+them'', we should /not/ resolve us. And
since the original pw cannot be conflicted (it consists of prim patches
only) we can disregard it. This leaves only them'' which is what we pass to
standardResolution to generate the markup, along with its full context,
consisting of (common +>+ us +>+ pw).

The resulting "resolution" goes on top, leading to our final unrecorded
state U'':

         U''
         |
        res
         |
         U'
        / \
      pw' them''
      /     \
     T       U
    / \     /
 us' them' pw
  /     \ /
 Y       R
  \     /
 them  us
    \ /
     X

In case the patches we pull are in conflict with local /unrecorded/ changes
(i.e. pw), we want to warn the user about that and allow them to cancel the
operation. The reason is that it is hard to reconstruct the original
unrecorded changes when they are messed up with conflict resolution markup.
To see if this is the case we check whether pw' has conflicts. As an extra
precaution we backup any conflicted files, so the user can refer to them to
restore things or compare in a diff viewer.

The patches we return are what we need to update U to U'' i.e. them''+>+res.
The new pending patch starts out at the new tentative state, so as explained
above, we set it to pw'+>+res, and again rely on sifting to commute out and
drop anything we don't need.

TODO: We should return a properly coerced @Repository 'RW p wU wR@.
-}

tentativelyMergePatches_ :: (RepoPatch p, ApplyState p ~ Tree)
                         => MakeChanges
                         -> Repository 'RW p wU wR -> String
                         -> AllowConflicts
                         -> WantGuiPause
                         -> Reorder
                         -> DiffOpts
                         -> Fork (PatchSet p)
                                 (FL (PatchInfoAnd p))
                                 (FL (PatchInfoAnd p)) Origin wR wY
                         -> IO (Sealed (FL (PrimOf p) wU))
tentativelyMergePatches_ mc _repo cmd allowConflicts wantGuiPause
  reorder diffingOpts@DiffOpts{..} (Fork context us them) = do
    (them' :/\: us') <-
      return $ merge (progressFL "Merging us" us :\/: progressFL "Merging them" them)
    pw <- unrecordedChanges diffingOpts _repo Nothing
    -- Note: we use anonymous here to wrap the unrecorded changes.
    -- This is benign because we only retain the effect of the results
    -- of the merge (pw' and them'').
    anonpw <- n2pia `fmap` anonymous pw
    pw' :/\: them'' <- return $ merge (them' :\/: anonpw :>: NilFL)
    let them''content = concatFL $ mapFL_FL (patchcontents . hopefully) them''
        no_conflicts_in_them =
          not $ haveConflicts $ patchsetConflictResolutions $
          slightlyOptimizePatchset (appendPSFL context them)
        conflicts =
          let us'' = us' +>+ pw' in
          -- This optimization is valid only if @them@ didn't have
          -- (unresolved) conflicts in the first place
          if lengthFL us'' < lengthFL them'' && no_conflicts_in_them then
            standardResolution
              (patchSet2RL context +<<+ them)
              (progressRL "Examining patches for conflicts" $ reverseFL us'')
          else
            standardResolution
              (patchSet2RL context +<<+ us :<: anonpw)
              (progressRL "Examining patches for conflicts" $ reverseFL them'')

    debugMessage "Checking for conflicts..."
    when (allowConflicts == YesAllowConflicts MarkConflicts) $
        mapM_ backupByCopying $
        map (anchorPath (repoLocation _repo)) $
        conflictedPaths conflicts

    debugMessage "Announcing conflicts..."
    have_conflicts <- announceConflicts cmd allowConflicts conflicts

    debugMessage "Checking for unrecorded conflicts..."
    let pw'content = concatFL $ mapFL_FL (patchcontents . hopefully) pw'
    case listConflictedFiles pw'content of
        [] -> return ()
        fs -> do
          ePutDocLn $ vcat $ map redText $
            "You have conflicting unrecorded changes to:" : map displayPath fs
          -- we catch "hIsTerminalDevice: illegal operation (handle is closed)"
          -- which can be thrown when we apply patches remotely (i.e. during push)
          confirmed <- promptYorn "Proceed?" `catchIOError` (\e ->
            if isIllegalOperationErrorType (ioeGetErrorType e)
              then return True
              else ioError e)
          unless confirmed $ do
            putStrLn "Cancelled."
            exitSuccess

    debugMessage "Reading working tree..."
    working <- readUnrecorded _repo withIndex Nothing

    debugMessage "Working out conflict markup..."
    Sealed resolution <-
      if have_conflicts then
        case allowConflicts of
          YesAllowConflicts (ExternalMerge merge_cmd) ->
            externalResolution diffAlg working merge_cmd wantGuiPause
              (effect us +>+ pw) (effect them) them''content
          YesAllowConflicts NoResolveConflicts -> return $ seal NilFL
          YesAllowConflicts MarkConflicts -> return $ mangled conflicts
          NoAllowConflicts -> error "impossible" -- was handled in announceConflicts
      else return $ seal NilFL

    debugMessage "Adding patches to the inventory and writing new pending..."
    when (mc == MakeChanges) $ do
        applyToTentativePristine _repo $ mkInvertible $
          progressFL "Applying patches to pristine" them'
        -- these two cases result in the same trees (that's the idea of
        -- merging), so we only operate on the set of patches and do the
        -- adaption of pristine and pending in the common code below
        _repo <- case reorder of
            NoReorder -> do
                tentativelyAddPatches_ DontUpdatePristine _repo NoUpdatePending them'
            Reorder -> do
                -- we do not actually remove any effect in the end, so
                -- it would be wrong to update the unrevert bundle or
                -- the working tree or pending
                _repo <- tentativelyRemovePatches_ DontUpdatePristineNorRevert _repo
                          NoUpdatePending us
                _repo <- tentativelyAddPatches_ DontUpdatePristine _repo
                          NoUpdatePending them
                tentativelyAddPatches_ DontUpdatePristine _repo NoUpdatePending us'
        setTentativePending _repo (effect pw' +>+ resolution)
    return $ seal (effect them''content +>+ resolution)

tentativelyMergePatches :: (RepoPatch p, ApplyState p ~ Tree)
                        => Repository 'RW p wU wR -> String
                        -> AllowConflicts
                        -> WantGuiPause
                        -> Reorder
                        -> DiffOpts
                        -> Fork (PatchSet p)
                                (FL (PatchInfoAnd p))
                                (FL (PatchInfoAnd p)) Origin wR wY
                        -> IO (Sealed (FL (PrimOf p) wU))
tentativelyMergePatches = tentativelyMergePatches_ MakeChanges

considerMergeToWorking :: (RepoPatch p, ApplyState p ~ Tree)
                       => Repository 'RW p wU wR -> String
                       -> AllowConflicts
                       -> WantGuiPause
                       -> Reorder
                       -> DiffOpts
                       -> Fork (PatchSet p)
                               (FL (PatchInfoAnd p))
                               (FL (PatchInfoAnd p)) Origin wR wY
                       -> IO (Sealed (FL (PrimOf p) wU))
considerMergeToWorking = tentativelyMergePatches_ DontMakeChanges
