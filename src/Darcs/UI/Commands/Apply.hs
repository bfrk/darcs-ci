--  Copyright (C) 2003-2005 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.UI.Commands.Apply
    ( apply, applyCmd
    , getPatchBundle -- used by darcsden
    ) where

import Darcs.Prelude

import System.Exit ( exitSuccess )
import Control.Monad ( unless, when )
import Data.Maybe ( catMaybes )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefullyM, info )
import Darcs.UI.Commands
    ( DarcsCommand(..), withStdOpts
    , putInfo
    , amInHashedRepository
    )
import Darcs.UI.Completion ( fileArgs )
import Darcs.UI.Flags
    ( DarcsFlag
    , changesReverse, verbosity, useCache
    , reorder, umask
    , fixUrl
    )
import Darcs.UI.Options ( (^), parseFlags, (?) )
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( toFilePath, AbsolutePath )
import Darcs.Repository
    ( Repository
    , AccessType(..)
    , SealedPatchSet
    , withRepoLock
    , readPatches
    , filterOutConflicts
    )
import Darcs.Patch.Set ( PatchSet, Origin )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Apply( ApplyState )
import Darcs.Patch.Info ( PatchInfo, displayPatchInfo )
import Darcs.Patch.Witnesses.Ordered
    ( Fork(..), (:>)(..)
    , mapFL, nullFL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Util.ByteString ( linesPS, unlinesPS, gzReadStdin )
import qualified Data.ByteString as B (ByteString, null, init)
import qualified Data.ByteString.Char8 as BC (last)

import Darcs.Util.HTTP ( Cachable(Uncachable) )
import Darcs.Util.File ( gzFetchFilePS )
import Darcs.UI.External
    ( verifyPS
    )
import Darcs.UI.Email ( readEmail )
import Darcs.Patch.Depends ( findCommon )
import Darcs.UI.ApplyPatches ( PatchApplier(..), StandardPatchApplier(..), PatchProxy )
import Darcs.UI.SelectChanges
    ( WhichChanges(..)
    , runSelection
    , selectionConfig
    )
import qualified Darcs.UI.SelectChanges as S
import Darcs.Patch.Bundle ( interpretBundle, parseBundle )
import Darcs.Util.Printer
    ( Doc, vcat, text
    , renderString
    , ($$)
    , vsep
    , formatWords
    )
import Darcs.Util.Tree( Tree )

applyDescription :: String
applyDescription = "Apply a patch bundle created by `darcs send'."

applyHelp :: Doc
applyHelp = vsep $ map formatWords
  [ [ "The `darcs apply` command takes a patch bundle and attempts to insert"
    , "it into the current repository.  In addition to invoking it directly"
    , "on bundles created by `darcs send`, it is used internally by `darcs"
    , "push` on the remote end of an SSH connection."
    ]
  , [ "If no file is supplied, the bundle is read from standard input."
    ]
  , [ "If given an email instead of a patch bundle, Darcs will look for the"
    , "bundle as a MIME attachment to that email.  Currently this will fail"
    , "if the MIME boundary is rewritten, such as in Courier and Mail.app."
    ]
  , [ "If gpg(1) is installed, you can use `--verify pubring.gpg` to reject"
    , "bundles that aren't signed by a key in `pubring.gpg`."
    ]
  , [ "If `--test` is supplied and a test is defined (see `darcs setpref`), the"
    , "bundle will be rejected if the test fails after applying it."
    ]
  , [ "Unlike most Darcs commands, `darcs apply` defaults to `--all`.  Use the"
    , "`--interactive` option to pick which patches to apply from a bundle."
    ]
  , [ "A patch bundle may introduce unresolved conflicts with existing"
    , "patches or with the working tree.  By default, Darcs will refuse to"
    , "apply conflicting patches (`--no-allow-conflicts`)."
    ]
  , [ "The `--mark-conflicts` option instructs Darcs to allow conflicts and"
    , "try to add conflict markup in your working tree. Note that this may"
    , "(partly) fail, because some conflicts cannot be marked, such as e.g."
    , "conflicts between two adds of the same file. In this case Darcs will"
    , "warn you and display the conflicting changes instead. When Darcs"
    , "detects conflicts with unrecorded changes, it will give you an extra"
    , "warning and prompts you to confirm that you want to continue. This is"
    , "because your original unrecorded changes cannot be automatically"
    , "restored by Darcs."
    ]
  , [ "Note that conflict markup is something Darcs adds to your working tree"
    , "files. Nevertheless, you can always re-construct it using"
    , "`darcs mark-conflicts`."
    ]
  , [ "The `--external-merge` option lets you resolve conflicts"
    , "using an external merge tool.  In the option, `%a` is replaced with"
    , "the common ancestor (merge base), `%1` with the first version, `%2`"
    , "with the second version, and `%o` with the path where your resolved"
    , "content should go.  For example, to use the xxdiff visual merge tool"
    , "you'd specify: `--external-merge='xxdiff -m -O -M %o %1 %a %2'`"
    ]
  , [ "The `--allow-conflicts` option allows conflicts but does not add"
    , "conflict markup. This is useful when you want to treat a repository as"
    , "just a bunch of patches, such as using `darcs pull --union` to download"
    , "all of your co-workers' patches before going offline. Again, conflict"
    , "markup can be added at any time later on using `darcs mark-conflicts`."
    ]
  , [ "For more information on conflicts in Darcs and how to resolve them,"
    , "see the help on `darcs mark-conflicts`."
    ]
  ]

stdindefault :: a -> [String] -> IO [String]
stdindefault _ [] = return ["-"]
stdindefault _ x = return x

apply :: DarcsCommand
apply = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "apply"
    , commandHelp = applyHelp
    , commandDescription = applyDescription
    , commandExtraArgs = 1
    , commandExtraArgHelp = ["<PATCHFILE>"]
    , commandCommand = applyCmd StandardPatchApplier
    , commandPrereq = amInHashedRepository
    , commandCompleteArgs = fileArgs
    , commandArgdefaults = const stdindefault
    , commandOptions = applyOpts
    }
  where
    applyBasicOpts
      = O.verify
      ^ O.reorder
      ^ O.interactive
      ^ O.dryRunXml
      ^ O.matchSeveral
      ^ O.conflictsNo
      ^ O.testChanges
      ^ O.repoDir
      ^ O.diffAlgorithm
    applyAdvancedOpts
      = O.setScriptsExecutable
      ^ O.umask
      ^ O.changesReverse
      ^ O.pauseForGui
    applyOpts = applyBasicOpts `withStdOpts` applyAdvancedOpts

applyCmd :: PatchApplier pa
         => pa
         -> (AbsolutePath, AbsolutePath)
         -> [DarcsFlag]
         -> [String]
         -> IO ()
applyCmd patchApplier (_,orig) opts args =
  withRepoLock (useCache ? opts) (umask ? opts) $
  repoJob patchApplier $ \patchProxy repository -> do
    bundle <- readBundle args
    applyCmdCommon patchApplier patchProxy opts bundle repository
  where
    readBundle ["-"] = do
      -- For users who try out 'darcs apply' without any arguments.
      -- FIXME apparently some magic behind the scenes causes an empty argument
      -- list to be converted to a single "-". This is quite obscure and should
      -- be removed.
      putInfo opts $ text "reading patch bundle from stdin..."
      gzReadStdin
    readBundle [""] = fail "Empty filename argument given to apply!"
    readBundle [unfixed_filename] = do
      patchesfile <- fixUrl orig unfixed_filename
      gzFetchFilePS (toFilePath patchesfile) Uncachable
    readBundle _ = error "impossible case"

applyCmdCommon
    :: forall pa p wR wU
     . (PatchApplier pa, RepoPatch p, ApplyState p ~ Tree)
    => pa
    -> PatchProxy p
    -> [DarcsFlag]
    -> B.ByteString
    -> Repository 'RW p wU wR
    -> IO ()
applyCmdCommon patchApplier patchProxy opts bundle repository = do
  us <- readPatches repository
  Sealed them <- either fail return =<< getPatchBundle opts us bundle
  Fork common us' them' <- return $ findCommon us them

  -- all patches in them' need to be available; check that
  let check :: PatchInfoAnd p wX wY -> Maybe PatchInfo
      check p = case hopefullyM p of
        Nothing -> Just (info p)
        Just _ -> Nothing
      bad = catMaybes (mapFL check them')
  unless (null bad) $
    fail $
    renderString $
      (vcat $ map displayPatchInfo bad) $$ text "" $$
      text "Cannot apply this bundle. We are missing the above patches."

  (hadConflicts, Sealed their_ps)
    <- if O.conflictsNo ? opts == Nothing -- skip conflicts
        then filterOutConflicts repository (O.useIndex ? opts) us' them'
        else return (False, Sealed them')
  when hadConflicts $ putStrLn "Skipping some patches which would cause conflicts."
  when (nullFL their_ps) $
       do if hadConflicts
           then putStrLn ("All new patches of the bundle cause conflicts.  " ++
                          "Nothing to do.") >> exitSuccess
           else putStrLn ("All these patches have already been applied.  " ++
                          "Nothing to do.") >> when (reorder ? opts /= O.Reorder) exitSuccess
          
  let direction = if changesReverse ? opts then FirstReversed else First
      selection_config = selectionConfig direction "apply" (patchSelOpts opts) Nothing Nothing
  (to_be_applied :> _) <- runSelection their_ps selection_config
  applyPatches patchApplier patchProxy "apply" opts repository (Fork common us' to_be_applied)

getPatchBundle :: RepoPatch p
               => [DarcsFlag]
               -> PatchSet p Origin wR
               -> B.ByteString
               -> IO (Either String (SealedPatchSet p Origin))
getPatchBundle opts us fps = do
    let opt_verify = parseFlags O.verify opts
    mps <- verifyPS opt_verify $ readEmail fps
    mops <- verifyPS opt_verify fps
    case (mps, mops) of
      (Nothing, Nothing) ->
          return $ Left "Patch bundle not properly signed, or gpg failed."
      (Just bundle, Nothing) -> return $ parseAndInterpretBundle us bundle
      (Nothing, Just bundle) -> return $ parseAndInterpretBundle us bundle
      -- We use careful_scan_bundle only below because in either of the two
      -- above case we know the patch was signed, so it really shouldn't
      -- need stripping of CRs.
      (Just ps1, Just ps2) -> case careful_scan_bundle ps1 of
                              Left _ -> return $ careful_scan_bundle ps2
                              Right x -> return $ Right x
          where careful_scan_bundle bundle =
                    case parseAndInterpretBundle us bundle of
                    Left e -> case parseAndInterpretBundle us $ stripCrPS bundle of
                              Right x -> Right x
                              _ -> Left e
                    x -> x
                stripCrPS :: B.ByteString -> B.ByteString
                stripCrPS bundle = unlinesPS $ map stripline $ linesPS bundle
                stripline p | B.null p = p
                            | BC.last p == '\r' = B.init p
                            | otherwise = p

parseAndInterpretBundle :: RepoPatch p
                        => PatchSet p Origin wR
                        -> B.ByteString
                        -> Either String (SealedPatchSet p Origin)
parseAndInterpretBundle us content = do
    Sealed bundle <- parseBundle content
    Sealed <$> interpretBundle us bundle

patchSelOpts :: [DarcsFlag] -> S.PatchSelectionOptions
patchSelOpts flags = S.PatchSelectionOptions
    { S.verbosity = verbosity ? flags
    , S.matchFlags = parseFlags O.matchSeveral flags
    , S.interactive = maybeIsInteractive flags
    , S.selectDeps = O.PromptDeps -- option not supported, use default
    , S.withSummary = O.NoSummary -- option not supported, use default
    }

maybeIsInteractive :: [DarcsFlag] -> Bool
maybeIsInteractive = maybe False id . parseFlags O.interactive
