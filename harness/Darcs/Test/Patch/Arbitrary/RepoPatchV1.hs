-- Copyright (C) 2002-2003,2007 David Roundy
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

{-# LANGUAGE UndecidableInstances #-}
module Darcs.Test.Patch.Arbitrary.RepoPatchV1 (Patch) where

import Prelude ()
import Darcs.Prelude

import Test.QuickCheck
import Control.Exception ( try, evaluate, SomeException )
import Control.Monad ( liftM, liftM2, liftM3, liftM4, replicateM )
import System.IO.Unsafe

import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( pack )

import Darcs.Patch
import Darcs.Patch.Annotate
import Darcs.Patch.V1 ()
import Darcs.Patch.V1.Core ( RepoPatchV1(..) )
import qualified Darcs.Patch.V1.Prim as V1 ( Prim(..) )
import Darcs.Patch.Prim.V1.Core ( Prim(..) )
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed ( Sealed(Sealed), unseal, mapSeal, Sealed2(..) )
import Darcs.Patch.Witnesses.Unsafe

import Darcs.Util.Path  ( AnchoredPath, floatPath, catPaths )

-- This definitely feels a bit weird to be importing Properties here, and
-- probably means we want to move this elsewhere, but Darcs.Test.Patch.Check is
-- already taken with something apparently only semi-related
import Darcs.Test.Patch.Properties.Check( checkAPatch )
import Darcs.Test.Patch.Arbitrary.Generic ( MightHaveDuplicate, ArbitraryPrim,PrimBased(..) )
import Darcs.Test.Patch.Arbitrary.RepoPatch
import Darcs.Test.Patch.Merge.Checked ( CheckedMerge(..) )
import Darcs.Test.Patch.RepoModel ( RepoState, ModelOf )
import Darcs.Test.Patch.WithState ( PropagateShrink(..) )

type Patch = RepoPatchV1 V1.Prim

pp :: Prim wX wY -> Patch wX wY
pp = PP . V1.Prim

class ArbitraryP p where
    arbitraryP :: Gen (Sealed (p wX))

instance
  (Annotate prim, ArbitraryPrim prim, PrimPatch prim, ApplyState prim ~ RepoState (ModelOf prim))
  => ArbitraryRepoPatch (RepoPatchV1 prim)
  where

    notRepoPatchV1 = Nothing

instance PrimPatch prim => CheckedMerge (RepoPatchV1 prim) where
  validateMerge v =
    case unsafePerformIO (try (evaluate v)) of
      Left (_ :: SomeException) -> Nothing
      Right x -> Just x

{-
TODO: there is a lot of overlap in testing between between this module
and Darcs.Test.Patch.QuickCheck

This module tests Prim and V1 patches, and Darcs.Test.Patch.QuickCheck
tests Prim and V2 patches

This module's generator covers a wider set of patch types, but is less
likely to generate conflicts than Darcs.Test.Patch.QuickCheck.

Until this is cleaned up, we take some care that the Arbitrary instances
do not overlap and are only used for tests from the respective
modules.

(There are also tests in other modules that probably depend on the
Arbitrary instances in this module.)
-}

instance Arbitrary (Sealed (Prim wX)) where
    arbitrary = arbitraryP

instance Arbitrary (Sealed ((Prim :> Prim) wX)) where
    arbitrary = arbitraryP

instance Arbitrary (Sealed (FL Patch wX)) where
    arbitrary = arbitraryP

instance Arbitrary (Sealed2 (Prim :> Prim)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch :\/: FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch :> FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch :> FL Patch :> FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP


instance (ArbitraryP p1, ArbitraryP p2) => ArbitraryP (p1 :> p2) where
    arbitraryP = do Sealed p1 <- arbitraryP
                    Sealed p2 <- arbitraryP
                    return (Sealed (p1 :> p2))

instance (ArbitraryP p1, ArbitraryP p2) => ArbitraryP (p1 :\/: p2) where
    arbitraryP = do Sealed p1 <- arbitraryP
                    Sealed p2 <- arbitraryP
                    return (Sealed (unsafeCoercePEnd p1 :\/: p2))

instance ArbitraryP (FL Patch) where
    arbitraryP = sized arbpatch

instance ArbitraryP Prim where
    arbitraryP = onepatchgen

instance MightHaveDuplicate (RepoPatchV1 prim)

type instance ModelOf (RepoPatchV1 prim) = ModelOf prim

instance (PrimPatch prim, ArbitraryPrim prim, PropagateShrink prim prim) => PrimBased (RepoPatchV1 prim) where
  type OnlyPrim (RepoPatchV1 prim) = prim
  primEffect prim = prim :>: NilFL
  liftFromPrim = PP

hunkgen :: Gen (Sealed (Prim wX))
hunkgen = do
  i <- frequency [(1,choose (0,5)),(1,choose (0,35)),
                  (2,return 0),(3,return 1),(2,return 2),(1,return 3)]
  j <- frequency [(1,choose (0,5)),(1,choose (0,35)),
                  (2,return 0),(3,return 1),(2,return 2),(1,return 3)]
  if i == 0 && j == 0 then hunkgen
    else Sealed <$>
            liftM4 hunk filepathgen linenumgen
                (replicateM i filelinegen)
                (replicateM j filelinegen)

tokreplacegen :: Gen (Sealed (Prim wX))
tokreplacegen = do
  f <- filepathgen
  o <- tokengen
  n <- tokengen
  if o == n
     then return $ Sealed $ tokreplace f "A-Za-z" "old" "new"
     else return $ Sealed $ tokreplace f "A-Za-z_" o n

twofilegen :: (forall wY . AnchoredPath -> AnchoredPath -> Prim wX wY) -> Gen (Sealed (Prim wX))
twofilegen p = do
  n1 <- filepathgen
  n2 <- filepathgen
  if n1 /= n2 && checkAPatch (p n1 n2)
     then return $ Sealed $ p n1 n2
     else twofilegen p

chprefgen :: Gen (Sealed (Prim wX))
chprefgen = do
  f <- oneof [return "color", return "movie"]
  o <- tokengen
  n <- tokengen
  if o == n then return $ Sealed $ changepref f "old" "new"
            else return $ Sealed $ changepref f o n

simplepatchgen :: Gen (Sealed (Prim wX))
simplepatchgen = frequency [(1,liftM (Sealed . addfile) filepathgen),
                            (1,liftM (Sealed . adddir) filepathgen),
                            (1,liftM3 (\x y z -> Sealed (binary x y z)) filepathgen arbitrary arbitrary),
                            (1,twofilegen move),
                            (1,tokreplacegen),
                            (1,chprefgen),
                            (7,hunkgen)
                           ]

onepatchgen :: Gen (Sealed (Prim wX))
onepatchgen = oneof [simplepatchgen, mapSeal (invert . unsafeCoerceP) `fmap` simplepatchgen]

norecursgen :: Int -> Gen (Sealed (FL Patch wX))
norecursgen 0 = mapSeal (\p -> pp p :>: NilFL) `fmap` onepatchgen
norecursgen n = oneof [mapSeal (\p -> pp p :>: NilFL) `fmap` onepatchgen,flatcompgen n]

arbpatch :: Int -> Gen (Sealed (FL Patch wX))
arbpatch 0 = mapSeal (\p -> pp p :>: NilFL) `fmap` onepatchgen
arbpatch n = frequency [(3,mapSeal (\p -> pp p :>: NilFL) `fmap` onepatchgen),
                        (2,flatcompgen n),
                        (0,rawMergeGen n),
                        (0,mergegen n),
                        (1,mapSeal (\p -> pp p :>: NilFL) `fmap` onepatchgen)
                       ]

rawMergeGen :: Int -> Gen (Sealed (FL Patch wX))
rawMergeGen n =   do Sealed p1 <- arbpatch len
                     Sealed p2 <- arbpatch len
                     if checkAPatch (invert p1:>:p2:>:NilFL) &&
                        checkAPatch (invert p2:>:p1:>:NilFL)
                        then case merge (p2 :\/: p1) of
                             _ :/\: p2' -> return (Sealed (unsafeCoercePStart p2'))
                        else rawMergeGen n
    where len = if n < 15 then n`div`3 else 3

mergegen :: Int -> Gen (Sealed (FL Patch wX))
mergegen n = do
  Sealed p1 <- norecursgen len
  Sealed p2 <- norecursgen len
  if checkAPatch (invert p1:>:p2:>:NilFL) &&
         checkAPatch (invert p2:>:p1:>:NilFL)
     then case merge (p2:\/:p1) of
          _ :/\: p2' ->
              if checkAPatch (p1+>+p2')
              then return $ Sealed $ p1+>+p2'
              else error "impossible case"
     else mergegen n
  where len = if n < 15 then n`div`3 else 3

instance Arbitrary B.ByteString where
    arbitrary = liftM BC.pack arbitrary

flatlistgen :: Int -> Gen (Sealed (FL Patch wX))
flatlistgen 0 = return $ Sealed NilFL
flatlistgen n = do Sealed x <- onepatchgen
                   Sealed xs <- flatlistgen (n-1)
                   return (Sealed (pp x :>: xs))

flatcompgen :: Int -> Gen (Sealed (FL Patch wX))
flatcompgen n = do
  Sealed ps <- flatlistgen n
  let myp = regularizePatches $ ps
  if checkAPatch myp
     then return $ Sealed myp
     else flatcompgen n

-- resize to size 25, that means we'll get line numbers no greater
-- than 1025 using QuickCheck 2.1
linenumgen :: Gen Int
linenumgen = frequency [(1,return 1), (1,return 2), (1,return 3),
                    (3,liftM (\n->1+abs n) (resize 25 arbitrary)) ]

tokengen :: Gen String
tokengen = oneof [return "hello", return "world", return "this",
                  return "is", return "a", return "silly",
                  return "token", return "test"]

toklinegen :: Gen String
toklinegen = liftM unwords $ replicateM 3 tokengen

filelinegen :: Gen B.ByteString
filelinegen = liftM BC.pack $
              frequency [(1,map fromSafeChar `fmap` arbitrary),(5,toklinegen),
                         (1,return ""), (1,return "{"), (1,return "}") ]

newtype SafeChar = SS Char
instance Arbitrary SafeChar where
    arbitrary = oneof $ map (return . SS) (['a'..'z']++['A'..'Z']++['1'..'9']++"0")

fromSafeChar :: SafeChar -> Char
fromSafeChar (SS s) = s

filepathgen :: Gen AnchoredPath
filepathgen =
  frequency
    [ (1, return $ floatPath "test")
    , (1, return $ floatPath "hello")
    , (1, return $ floatPath "world")
    , (1, floatPath `fmap` map fromSafeChar `fmap` arbitrary)
    , (1, liftM2 catPaths filepathgen filepathgen)
    ]

regularizePatches :: FL Patch wX wY -> FL Patch wX wY
regularizePatches patches = rpint (unsafeCoerceP NilFL) patches
    where -- this reverses the list, which seems odd and causes
          -- the witness unsafety
          rpint :: FL Patch wX wY -> FL Patch wA wB -> FL Patch wX wY
          rpint ok_ps NilFL = ok_ps
          rpint ok_ps (p:>:ps) =
            if checkAPatch (unsafeCoerceP p:>:ok_ps)
            then rpint (unsafeCoerceP p:>:ok_ps) ps
            else rpint ok_ps ps

