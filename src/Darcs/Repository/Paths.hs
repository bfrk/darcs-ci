-- everything here has type String/FilePath
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Darcs.Repository.Paths where

import Darcs.Prelude
import Darcs.Util.Cache ( HashedDir(..), hashedDir )
import Darcs.Util.Global ( darcsdir )
import System.FilePath.Posix( (</>) )

makeDarcsdirPath :: String -> String
makeDarcsdirPath name = darcsdir </> name

-- | Location of the lock file.
lockPath = makeDarcsdirPath "lock"

-- | Location of the prefs directory.
prefsDirPath = makeDarcsdirPath "prefs"

-- | Location of the (one and only) head inventory.
hashedInventoryPath = makeDarcsdirPath "hashed_inventory"

-- | Location of the (one and only) tentative head inventory.
tentativeHashedInventoryPath = makeDarcsdirPath "tentative_hashed_inventory"

-- | Location of parent inventories.
inventoriesDir = hashedDir HashedInventoriesDir
inventoriesDirPath = makeDarcsdirPath inventoriesDir

-- | Location of the (one and only) tentative pristine root
tentativePristinePath = makeDarcsdirPath "tentative_pristine"

-- | Location of pristine trees.
pristineDir = hashedDir HashedPristineDir
pristineDirPath = makeDarcsdirPath pristineDir

-- | Location of patches.
patchesDir = hashedDir HashedPatchesDir
patchesDirPath = makeDarcsdirPath patchesDir

-- | Location of index files.
indexPath = darcsdir </> "index"
indexInvalidPath = darcsdir </> "index_invalid"

-- | Location of the rebase patch
rebasePath = makeDarcsdirPath "rebase"
tentativeRebasePath = makeDarcsdirPath "rebase.tentative"

-- | Location of format file
formatPath = makeDarcsdirPath "format"

-- | Location of pending files
pendingPath = patchesDirPath </> "pending"
tentativePendingPath = patchesDirPath </> "pending.tentative"

-- | Location of unrevert bundle.
unrevertPath = patchesDirPath </> "unrevert"
tentativeUnrevertPath = patchesDirPath </> "unrevert.tentative"

-- | Location of old style (unhashed) files and directories.
oldPristineDirPath = makeDarcsdirPath "pristine"
oldCurrentDirPath = makeDarcsdirPath "current"
oldCheckpointDirPath = makeDarcsdirPath "checkpoints"
oldInventoryPath = makeDarcsdirPath "inventory"
oldTentativeInventoryPath = makeDarcsdirPath "tentative_inventory"
