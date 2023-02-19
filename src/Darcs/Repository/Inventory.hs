module Darcs.Repository.Inventory
    ( module Darcs.Repository.Inventory.Format
    , readPatchesUsingSpecificInventory
    , readPatchesFromInventory
    , readPatchesFromInventoryEntries
    , readSinglePatch
    , readInventoryPrivate
    , writeInventory
    , writePatchIfNecessary
    , writeHashFile
    ) where

import Darcs.Prelude

import Control.Exception ( catch )
import Control.Monad ( unless )
import System.FilePath.Posix ( (</>) )
import System.IO ( hPutStrLn, stderr )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Patch ( RepoPatch, readPatch, showPatch )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( PatchInfo, displayPatchInfo, piName )
import Darcs.Patch.PatchInfoAnd
    ( PatchInfoAnd
    , PatchInfoAndG
    , createHashed
    , extractHash
    , info
    , patchInfoAndPatch
    )
import Darcs.Patch.Read ( ReadPatch )
import Darcs.Patch.Set ( Origin, PatchSet(..), SealedPatchSet, Tagged(..) )
import Darcs.Patch.Show ( ShowPatchFor(..) )
import Darcs.Patch.Witnesses.Ordered ( RL(..), mapRL )
import Darcs.Patch.Witnesses.Sealed ( Sealed(..), mapSeal, seal, unseal )
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Repository.Flags ( Compression )
import Darcs.Repository.InternalTypes ( Repository, repoCache, repoLocation )
import Darcs.Repository.Inventory.Format
import Darcs.Util.Cache
    ( Cache
    , fetchFileUsingCache
    , peekInCache
    , speculateFilesUsingCache
    , writeFileUsingCache
    )
import Darcs.Util.File ( Cachable(Uncachable), gzFetchFilePS )
import Darcs.Util.Printer ( Doc, renderPS, renderString, text, ($$) )
import Darcs.Util.Progress ( debugMessage, finishedOneIO )

-- | Read a 'PatchSet' starting with a specific inventory inside a 'Repository'.
readPatchesUsingSpecificInventory :: (PatchListFormat p, ReadPatch p)
                                  => FilePath
                                  -> Repository rt p wU wR
                                  -> IO (PatchSet p Origin wS)
readPatchesUsingSpecificInventory invPath repo = do
  let repodir = repoLocation repo
  Sealed ps <-
    catch
      (readInventoryPrivate (repodir </> invPath) >>=
       readPatchesFromInventory (repoCache repo))
      (\e -> hPutStrLn stderr ("Invalid repository: " ++ repodir) >> ioError e)
  return $ unsafeCoerceP ps

-- | Read a complete 'PatchSet' from a 'Cache', by following the chain of
-- 'Inventory's, starting with the given one.
readPatchesFromInventory :: (PatchListFormat p, ReadPatch p)
                         => Cache
                         -> Inventory
                         -> IO (SealedPatchSet p Origin)
readPatchesFromInventory cache = parseInv
  where
    parseInv :: (PatchListFormat p, ReadPatch p)
             => Inventory
             -> IO (SealedPatchSet p Origin)
    parseInv (Inventory Nothing ris) =
        mapSeal (PatchSet NilRL) <$> readPatchesFromInventoryEntries cache ris
    parseInv (Inventory (Just h) []) =
        -- TODO could be more tolerant and create a larger PatchSet
        error $ "bad inventory " ++ encodeValidHash h ++ " (no tag) in parseInv!"
    parseInv (Inventory (Just h) (t : ris)) = do
        Sealed ts <- delaySealed (read_ts t h)
        Sealed ps <- delaySealed (readPatchesFromInventoryEntries cache ris)
        return $ seal $ PatchSet ts ps

    read_ts :: (PatchListFormat p, ReadPatch p) => InventoryEntry
            -> InventoryHash -> IO (Sealed (RL (Tagged p) Origin))
    read_ts tag0 h0 = do
        contents <- unsafeInterleaveIO $ readTaggedInventory h0
        let is = case contents of
                    Inventory (Just _) (_ : ris0) -> ris0
                    Inventory Nothing ris0 -> ris0
                    Inventory (Just _) [] -> error "inventory without tag!"
        Sealed ts <-
            delaySealed $
                case contents of
                    Inventory (Just h') (t' : _) -> read_ts t' h'
                    Inventory (Just _) [] -> error "inventory without tag!"
                    Inventory Nothing _ -> return $ seal NilRL
        Sealed ps <- delaySealed (readPatchesFromInventoryEntries cache is)
        Sealed tag00 <- read_tag tag0
        return $ seal $ ts :<: Tagged ps tag00 (Just h0)

    read_tag :: (PatchListFormat p, ReadPatch p) => InventoryEntry
             -> IO (Sealed (PatchInfoAnd p wX))
    read_tag (i, h) =
        mapSeal (patchInfoAndPatch i) <$> createHashed h (readSinglePatch cache i)

    readTaggedInventory :: InventoryHash -> IO Inventory
    readTaggedInventory invHash = do
        (fileName, pristineAndInventory) <- fetchFileUsingCache cache invHash
        case parseInventory pristineAndInventory of
          Right r -> return r
          Left e -> fail $ unlines [unwords ["parse error in file", fileName],e]

-- | Read patches from a 'Cache' as specified by a list of 'InventoryEntry'.
readPatchesFromInventoryEntries :: ReadPatch np
                                => Cache
                                -> [InventoryEntry]
                                -> IO (Sealed (RL (PatchInfoAndG np) wX))
readPatchesFromInventoryEntries cache ris = read_patches (reverse ris)
  where
    read_patches [] = return $ seal NilRL
    read_patches allis@((i1, h1) : is1) =
        lift2Sealed (\p rest -> rest :<: i1 `patchInfoAndPatch` p) (rp is1)
                    (createHashed h1 (const $ speculateAndParse h1 allis i1))
      where
        rp [] = return $ seal NilRL
        rp [(i, h), (il, hl)] =
            lift2Sealed (\p rest -> rest :<: i `patchInfoAndPatch` p)
                        (rp [(il, hl)])
                        (createHashed h
                            (const $ speculateAndParse h (reverse allis) i))
        rp ((i, h) : is) =
            lift2Sealed (\p rest -> rest :<: i `patchInfoAndPatch` p)
                        (rp is)
                        (createHashed h (readSinglePatch cache i))

    lift2Sealed :: (forall wY wZ . q wY wZ -> p wX wY -> r wX wZ)
                -> IO (Sealed (p wX))
                -> (forall wB . IO (Sealed (q wB)))
                -> IO (Sealed (r wX))
    lift2Sealed f iox ioy = do
        Sealed x <- delaySealed iox
        Sealed y <- delaySealed ioy
        return $ seal $ f y x

    speculateAndParse h is i = speculate h is >> readSinglePatch cache i h

    speculate :: PatchHash -> [InventoryEntry] -> IO ()
    speculate pHash is = do
        already_got_one <- peekInCache cache pHash
        unless already_got_one $
            speculateFilesUsingCache cache (map snd is)

-- | We have to unseal and then reseal, otherwise the 'unsafeInterleaveIO' has
-- no effect.
delaySealed :: IO (Sealed (p wX)) -> IO (Sealed (p wX))
delaySealed = fmap (unseal seal) . unsafeInterleaveIO

-- | Read a single patch from a 'Cache', given its 'PatchInfo' and 'PatchHash'.
-- Fails with an error message if the patch file cannot be parsed.
readSinglePatch :: ReadPatch p
                => Cache
                -> PatchInfo -> PatchHash -> IO (Sealed (p wX))
readSinglePatch cache i h = do
    debugMessage $ "Reading patch file for: " ++ piName i
    (fn, ps) <- fetchFileUsingCache cache h
    case readPatch ps of
        Right p -> return p
        Left e -> fail $ unlines
            [ "Couldn't parse file " ++ fn
            , "which is patch"
            , renderString $ displayPatchInfo i
            , e
            ]

-- | Read an 'Inventory' from a file. Fails with an error message if
-- file is not there or cannot be parsed.
readInventoryPrivate :: FilePath -> IO Inventory
readInventoryPrivate path = do
    inv <- skipPristineHash <$> gzFetchFilePS path Uncachable
    case parseInventory inv of
      Right r -> return r
      Left e -> fail $ unlines [unwords ["parse error in file", path],e]

writeInventory :: RepoPatch p => String -> Cache -> Compression
               -> PatchSet p Origin wX -> IO InventoryHash
writeInventory tediousName cache compr = go
  where
    go :: RepoPatch p => PatchSet p Origin wX -> IO InventoryHash
    go (PatchSet ts ps) = do
      entries <- sequence $ mapRL (writePatchIfNecessary cache compr) ps
      content <- write_ts ts entries
      writeHashFile cache compr content
    write_ts NilRL entries = return $ showInventoryPatches (reverse entries)
    write_ts (tts :<: Tagged tps t maybeHash) entries = do
      -- if the Tagged has a hash, then we know that it has already been
      -- written; otherwise recurse without the tag
      parenthash <- maybe (go (PatchSet tts tps)) return maybeHash
      let parenthash_str = encodeValidHash parenthash
      finishedOneIO tediousName parenthash_str
      tag_entry <- writePatchIfNecessary cache compr t
      return $
        text ("Starting with inventory:\n" ++ parenthash_str) $$
        showInventoryPatches (tag_entry : reverse entries)

-- | Write a 'PatchInfoAnd' to disk and return an 'InventoryEntry' i.e. the
-- patch info and hash. However, if we patch already contains a hash, assume it
-- has already been written to disk at some point and merely return the info
-- and hash.
writePatchIfNecessary :: RepoPatch p => Cache -> Compression
                      -> PatchInfoAnd p wX wY -> IO InventoryEntry
writePatchIfNecessary c compr hp = infohp `seq`
    case extractHash hp of
        Right h -> return (infohp, h)
        Left p ->
          (infohp,) <$>
            writeHashFile c compr (showPatch ForStorage p)
  where
    infohp = info hp

-- | Wrapper around 'writeFileUsingCache' that takes a 'Doc' instead of a
-- 'ByteString'.
writeHashFile :: ValidHash h => Cache -> Compression -> Doc -> IO h
writeHashFile c compr d = writeFileUsingCache c compr (renderPS d)

