-- Copyright (C) 2007 David Roundy
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

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Darcs.Test.Patch.Examples.Set2
    ( primPermutables
    , primPatches
    , commutables
    , commutablesFL
    , repov2Commutables
    , repov2Mergeables
    , repov2Triples
    , repov2NonduplicateTriples
    , repov2Patches
    , repov2PatchLoopExamples
    ) where

import Darcs.Prelude

import qualified Data.ByteString.Char8 as BC ( pack )
import Data.Maybe ( catMaybes )
import Data.String ( IsString(..) )
import qualified Data.ByteString as B ( ByteString )

import Darcs.Patch ( hunk, invert )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.FromPrim ( fromAnonymousPrim )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Merge ( Merge, merge, mergeFL )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.V2 ( RepoPatchV2 )
import qualified Darcs.Patch.V2.Prim as V2
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Witnesses.Unsafe ( unsafeCoercePEnd, unsafeCoercePStart )
import Darcs.Util.Path ( AnchoredPath, makeName, unsafeFloatPath )

import Darcs.Test.Patch.Arbitrary.Generic ( notDuplicatestriple )
import Darcs.Test.Patch.Arbitrary.PatchTree
    ( Tree(..)
    , TreeWithFlattenPos(..)
    , canonizeTree
    , commutePairFromTWFP
    , commutePairFromTree
    , commuteTripleFromTree
    , getPairs
    , getTriples
    , mergePairFromCommutePair
    )
import Darcs.Test.Patch.Arbitrary.PrimV1 ()
import Darcs.Test.Patch.Arbitrary.RepoPatchV2 ()
import Darcs.Test.Patch.Types.Merged ( Merged )
import Darcs.Test.Patch.V1Model ( Content, V1Model, makeFile, makeRepo )
import Darcs.Test.Patch.WithState ( WithStartState(..) )

instance IsString AnchoredPath where
  fromString = unsafeFloatPath

type Prim2 = V2.Prim

type Patch = RepoPatchV2 Prim2

makeSimpleRepo :: String -> Content -> V1Model wX
makeSimpleRepo filename content =
  makeRepo [(either error id $ makeName filename, makeFile content)]

withStartState :: s wX -> p wX -> Sealed (WithStartState s p)
withStartState s p = seal (WithStartState s p)

tripleExamples :: [Sealed2 (Patch :> Patch :> Patch)]
tripleExamples =
  catMaybes
    [ commuteTripleFromTree seal2 $
      withStartState
        (makeSimpleRepo "file" [])
        (ParTree
           (SeqTree
              (hunk "file" 1 [] ["g"])
              (SeqTree
                 (hunk "file" 2 [] ["j"])
                 (SeqTree (hunk "file" 1 [] ["s"]) NilTree)))
           (SeqTree (hunk "file" 1 [] ["e"]) NilTree))
    , commuteTripleFromTree seal2 $
      withStartState
        (makeSimpleRepo "file" ["j"])
        (ParTree
           (SeqTree
              (hunk "file" 1 [] ["s"])
              (ParTree
                 (SeqTree (hunk "file" 2 ["j"] []) NilTree)
                 (SeqTree (hunk "file" 2 ["j"] []) NilTree)))
           (SeqTree (hunk "file" 1 ["j"] []) NilTree))
    ]

mergeExamples :: [Sealed2 (Patch :\/: Patch)]
mergeExamples = map (unseal2 (mergePairFromCommutePair seal2)) commuteExamples

commuteExamples :: [Sealed2 (Patch :> Patch)]
commuteExamples =
  catMaybes
    [ commutePairFromTWFP seal2 $
      withStartState (makeSimpleRepo "file" [])
      (TWFP 3
       (ParTree
        (SeqTree (hunk "file" 1 [] ["h"]) NilTree)
        (SeqTree (hunk "file" 1 [] ["b"])
          (SeqTree (hunk "file" 1 [] ["f"])
            (SeqTree (hunk "file" 1 [] ["v"])
              (SeqTree (hunk "file" 2 ["f"] []) NilTree))))))
    , commutePairFromTWFP seal2 $
      withStartState
      (makeSimpleRepo "file" ["f","s","d"])
      (TWFP 3
       (ParTree
        (SeqTree (hunk "file" 3 ["d"] []) NilTree)
        (ParTree
         (SeqTree (hunk "file" 1 ["f"] []) NilTree)
         (SeqTree (hunk "file" 1 ["f"] [])
           (SeqTree (hunk "file" 1 ["s","d"] [])
             (SeqTree (hunk "file" 1 [] ["v"]) NilTree))))))
{-  , commutePairFromTWFP seal2 $
      withStartState
      (makeSimpleRepo "file" ["f","u",
                               "s","d"])
      (TWFP 5
       (ParTree
        (SeqTree (hunk "file" 5 [] ["x"])
         (SeqTree (hunk "file" 4 ["d"] []) NilTree))
        (ParTree
         (SeqTree (hunk "file" 1 ["f","u"] []) NilTree)
         (SeqTree (hunk "file" 1 ["f"] [])
          (SeqTree (hunk "file" 1 ["u","s","d"] [])
           (SeqTree (hunk "file" 1 [] ["a"])
            (SeqTree (hunk "file" 1 ["a"] []) NilTree)))))))
-}
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" ["n","t","h"])
      (ParTree
       (SeqTree (hunk "file" 1 ["n","t","h"] [])
        NilTree)
       (SeqTree (hunk "file" 3 ["h"] [])
        (SeqTree (hunk "file" 1 ["n"] [])
         (SeqTree (hunk "file" 1 ["t"] []) NilTree))))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" [])
      (ParTree
       (SeqTree (hunk "file" 1 [] ["n"]) NilTree)
       (SeqTree (hunk "file" 1 [] ["i"])
                    (SeqTree (hunk "file" 1 [] ["i"]) NilTree)))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" [])
      (ParTree
       (SeqTree (hunk "file" 1 [] ["c"])
         (ParTree
           (SeqTree (hunk "file" 1 ["c"] ["r"]) NilTree)
           (SeqTree (hunk "file" 1 [] ["h"])
            (SeqTree (hunk "file" 1 [] ["d"]) NilTree))))
       (SeqTree (hunk "file" 1 [] ["f"]) NilTree))
    , commutePairFromTWFP seal2 $
      withStartState (makeSimpleRepo "file" [])
      (TWFP 1
      (ParTree
       (ParTree
        (SeqTree (hunk "file" 1 [] ["t"]) NilTree)
        (SeqTree (hunk "file" 1 [] ["t"]) NilTree))
       (SeqTree (hunk "file" 1 [] ["f"]) NilTree)))
    , commutePairFromTWFP seal2 $
      withStartState
        (makeSimpleRepo "file" ["f", " r", "c", "v"])
      (TWFP 4
       (ParTree
        (SeqTree (hunk "file" 3 ["c","v"] [])
           (ParTree
            (SeqTree (hunk "file" 2 ["r"] [])
             (SeqTree (hunk "fi le" 1 ["f"] []) NilTree))
            (SeqTree (hunk "file" 1 ["f","r"] [])
             (SeqTree (hunk "file" 1 [] ["y"]) NilTree))))
        (SeqTree (hunk "file" 4 ["v"] []) NilTree)))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" [])
      (ParTree
       (SeqTree (hunk "file" 1 [] ["z"]) NilTree)
       (ParTree
        (SeqTree (hunk "file" 1 [] ["f"]) NilTree)
        (ParTree
         (SeqTree (hunk "file" 1 [] ["r"]) NilTree)
         (SeqTree (hunk "file" 1 [] ["d"]) NilTree))))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" ["t","r","h"])
      (ParTree
       (ParTree
        (SeqTree (hunk "file" 1 ["t","r","h"] [])
                 NilTree)
        (SeqTree (hunk "file" 1 [] ["o"]) NilTree))
       (SeqTree (hunk "file" 1 ["t"] [])
        (SeqTree (hunk "file" 2 ["h"] []) NilTree)))
    , commutePairFromTWFP seal2 $
      withStartState (makeSimpleRepo "file" []) $
      TWFP 2
      (ParTree
       (SeqTree (hunk "file" 1 [] ["h"]) NilTree)
       (SeqTree (hunk "file" 1 [] ["y"])
        (SeqTree (hunk "file" 2 [] ["m"])
         (SeqTree (hunk "file" 1 [] ["v"]) NilTree))))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" [])
      (ParTree
       (SeqTree (hunk "file" 1 [] ["p"])
        (SeqTree (hunk "file" 1 ["p"] [])
         (SeqTree (hunk "file" 1 [] ["c"]) NilTree)))
       (SeqTree (hunk "file" 1 [] ["z"]) NilTree))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" [])
      (ParTree
       (SeqTree (hunk "file" 1 [] ["j" ])
        (SeqTree (hunk "file" 1 ["j"] []) NilTree))
       (SeqTree (hunk "file" 1 [] ["v"]) NilTree))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" [])
      (ParTree
       (SeqTree (hunk "file" 1 [] ["v"]) NilTree)
       (SeqTree (hunk "file" 1 [] ["j" ])
        (SeqTree (hunk "file" 1 ["j"] []) NilTree)))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" ["x","c"])
      (ParTree
       (SeqTree (hunk "file" 1 [] ["h"])
        (ParTree
         (SeqTree (hunk "file" 3 ["c"] []) NilTree)
         (SeqTree (hunk "file" 2 ["x"] [])
          (SeqTree (hunk "file" 1 [] ["j"]) NilTree))))
       (SeqTree (hunk "file" 1 [] ["l"]) NilTree))
    , commutePairFromTree seal2 $
      withStartState (makeSimpleRepo "file" [])
      (ParTree
       (SeqTree (hunk "file" 1 [] (packStringLetters "s")) NilTree)
       (SeqTree (hunk "file" 1 [] (packStringLetters "k"))
        (SeqTree (hunk "file" 1 (packStringLetters "k") [])
         (SeqTree (hunk "file" 1 [] (packStringLetters "m"))
          (SeqTree (hunk "file" 1 (packStringLetters "m") []) NilTree)))))
    ]

packStringLetters :: String -> [B.ByteString]
packStringLetters s = [ BC.pack [c] | c <- s ]

repov2PatchLoopExamples :: [Sealed (WithStartState V1Model (Tree Prim2))]
repov2PatchLoopExamples =
    [Sealed (WithStartState (makeSimpleRepo fx [])
     $ canonizeTree
     (ParTree
      (SeqTree (hunk fx 1 [] (packStringLetters "pkotufogbvdabnmbzajvolwviqebieonxvcvuvigkfgybmqhzuaaurjspd"))
       (ParTree
        (SeqTree (hunk fx 47 (packStringLetters "qhzu") (packStringLetters "zafybdcokyjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmh"))
         (ParTree
          (ParTree
           NilTree
           (ParTree
            (ParTree
             (ParTree
              (SeqTree (hunk fx 15 (packStringLetters "mbzajvolwviqebieonxvcvuvigkfgyb") (packStringLetters "vujnxnhvybvpouyciaabszfmgssezlwwjgnethvrpnfrkubphzvdgymjjoacppqps"))
               (ParTree
                NilTree
                (ParTree
                 (SeqTree (hunk fx 40 (packStringLetters "ssezlwwjgnethvrpnfrkubphzvdgymjjoacppqpsmzafybdcokyjskcgnvhkbz") (packStringLetters "wnesidpccwoiqiichxaaejdsyrhrusqljlcoro"))
                  (ParTree
                   (ParTree
                    (SeqTree (hunk fx 12 (packStringLetters "abnvujnxnhvybvpouyciaabszfmgwnesidpccwoiqii") (packStringLetters "czfdhqkipdstfjycqaxwnbxrihrufdeyneqiiiafwzlmg")) NilTree)
                    NilTree)
                   NilTree))
                 (SeqTree (hunk fx 25 [] (packStringLetters "dihgmsotezucqdgxczvcivijootyvhlwymbiueufnvpwpeukmskqllalfe")) NilTree))))
              (SeqTree (hunk fx 56 (packStringLetters "yjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmhaaurjsp") (packStringLetters "xldhrutyhcyaqeezwujiguawfyawjjqlirxshjddvq")) NilTree))
             (SeqTree (hunk fx 20 [] (packStringLetters "ooygwiyogqrqnytixqtmvdxx"))
              (SeqTree (hunk fx 26 (packStringLetters "yogqrqnytixqtmvdxxvolwviqebieonxvcvuvigkfgybmzafybdcokyjskcgnvhkbz") (packStringLetters "akhsmlbkdxnvfoikmiatfbpzdrsyykkpoxvvddeaspzxe"))
               (SeqTree (hunk fx 39 [] (packStringLetters "ji"))
                (ParTree
                 NilTree
                 (ParTree
                  NilTree
                  (ParTree
                   (ParTree
                    NilTree
                    (SeqTree (hunk fx 26 (packStringLetters "akhsmlbkdxnvfjioikmiatfbpzdrsyykkpoxvvddeaspzxepysaafnjjhcstgrczplxs") (packStringLetters "onjbhddskcj"))
                     (SeqTree (hunk fx 39 [] (packStringLetters "fyscunxxxjjtyqpfxeznhtwvlphmp")) NilTree)))
                   (ParTree
                    NilTree
                    (SeqTree (hunk fx 44 [] (packStringLetters "xcchzwmzoezxkmkhcmesplnjpqriypshgiqklgdnbmmkldnydiy"))
                     (ParTree
                      NilTree
                      (SeqTree (hunk fx 64 (packStringLetters "plnjpqriypshgiqklgdnbmmkldnydiymiatfbpzdrsyykkpoxvvddeaspzxepysaafn") (packStringLetters "anjlzfdqbjqbcplvqvkhwjtkigp")) NilTree)))))))))))
            (ParTree
             NilTree
             NilTree)))
          NilTree))
        NilTree))
      (ParTree
       NilTree
       (SeqTree (hunk fx 1 [] (packStringLetters "ti"))
        (SeqTree (hunk fx 1 (packStringLetters "t") (packStringLetters "ybcop"))
         (SeqTree (hunk fx 2 [] (packStringLetters "dvlhgwqlpaeweerqrhnjtfolczbqbzoccnvdsyqiefqitrqneralf"))
          (SeqTree (hunk fx 15 [] (packStringLetters "yairbjphwtnaerccdlfewujvjvmjakbc"))
           (SeqTree (hunk fx 51 [] (packStringLetters "xayvfuwaiiogginufnhsrmktpmlbvxiakjwllddkiyofyfw"))
            (ParTree
             NilTree
             NilTree)))))))))]
  where
      fx :: IsString a => a
      fx = "F"

quickhunk :: PrimPatch prim => Int -> String -> String -> prim wX wY
quickhunk l o n =
  hunk "test" l (map (\c -> BC.pack [c]) o) (map (\c -> BC.pack [c]) n)

primPermutables :: [(Prim2 :> Prim2 :> Prim2) wX wY]
primPermutables =
  [quickhunk 0 "e" "bo" :> quickhunk 3 "" "x" :> quickhunk 2 "f" "qljo"]

mergeables :: [(Prim2 :\/: Prim2) wX wY]
mergeables =
  [ quickhunk 1 "a" "b" :\/: quickhunk 1 "a" "c"
  , quickhunk 1 "a" "b" :\/: quickhunk 3 "z" "c"
  , quickhunk 0 "" "a" :\/: quickhunk 1 "" "b"
  , quickhunk 0 "a" "" :\/: quickhunk 1 "" "b"
  , quickhunk 0 "a" "" :\/: quickhunk 1 "b" ""
  , quickhunk 0 "" "a" :\/: quickhunk 1 "b" ""
  ]

mergeablesFL :: [(FL Prim2 :\/: FL Prim2) wX wY]
mergeablesFL = map (\(x :\/: y) -> (x :>: NilFL) :\/: (y :>: NilFL)) mergeables
  --  ++  [(quickhunk 1 "a" "b" :>: quickhunk 3 "z" "c" :>: NilFL)
  --  :\/: (quickhunk 1 "a" "z" :>: NilFL),
  --  (quickhunk 1 "a" "b" :>: quickhunk 1 "b" "c" :>: NilFL)
  --  :\/: (quickhunk 1 "a" "z" :>: NilFL)]

mergeable2commutable :: Invert p => (p :\/: p) wX wY -> (p :> p) wX wY
mergeable2commutable (x :\/: y) = (invert x) :> y

commutablesFL :: [(FL Prim2 :> FL Prim2) wX wY]
commutablesFL = map mergeable2commutable mergeablesFL

commutables :: [(Prim2 :> Prim2) wX wY]
commutables = map mergeable2commutable mergeables

primPatches :: [Sealed2 Prim2]
primPatches = concatMap mergeable2patches mergeables
  where
    mergeable2patches (x :\/: y) = [Sealed2 x, Sealed2 y]

repov2Patches :: [Sealed2 Patch]
repov2Patches = concatMap commutable2patches repov2Commutables
  where
    commutable2patches (Sealed2 (x :> y)) = [Sealed2 x, Sealed2 y]

typedMerge
  :: Merge p => (p :\/: p) wA wB -> (p wA (Merged wA wB), p wB (Merged wA wB))
typedMerge (p :\/: q) =
  case merge (p :\/: q) of
    (q' :/\: p') -> (unsafeCoercePEnd q', unsafeCoercePEnd p')

repov2Triples :: [Sealed2 (Patch :> Patch :> Patch)]
repov2Triples
  | oa <- fromAnonymousPrim $ quickhunk 1 "o" "aa"
  , oa2 <- fromAnonymousPrim $ quickhunk 1 "o" "aa"
  , a2 <- fromAnonymousPrim $ quickhunk 2 "a34" "2xx"
  , ob <- fromAnonymousPrim $ quickhunk 1 "o" "bb"
  , (ob', oa') <- typedMerge (oa :\/: ob)
  , (a2', _) <- typedMerge (ob' :\/: a2)
  , (a2'', _) <- typedMerge (oa2 :\/: a2') =
    [Sealed2 (ob' :> oa2 :> a2''), Sealed2 (oa' :> oa2 :> a2'')] ++
    tripleExamples ++ getTriples repov2FL

repov2NonduplicateTriples :: [Sealed2 (Patch :> Patch :> Patch)]
repov2NonduplicateTriples = filter (unseal2 notDuplicatestriple) repov2Triples

repov2FL :: FL Patch wX wX
repov2FL
  | oa <- fromAnonymousPrim $ quickhunk 1 "o" "a"
  , ps :/\: _ <-
      merge (oa :>: invert oa :>: nilFL :\/: oa :>: invert oa :>: nilFL) =
    oa :>: invert oa :>: oa :>: invert oa :>:
    unsafeCoercePEnd ps +>+ oa :>: invert oa :>: nilFL

repov2Commutables :: [Sealed2 (Patch :> Patch)]
repov2Commutables
  | oa <- fromAnonymousPrim $ quickhunk 1 "o" "a"
  , ob <- fromAnonymousPrim $ quickhunk 1 "o" "b"
  , _ :/\: ob' <- mergeFL (ob :\/: oa :>: invert oa :>: nilFL) =
    commuteExamples ++
    map (mapSeal2 mergeable2commutable) repov2Mergeables ++
    [Sealed2 (invert oa :> ob')] ++
    getPairs repov2FL

repov2Mergeables :: [Sealed2 (Patch :\/: Patch)]
repov2Mergeables
  | oa <- fromAnonymousPrim $ quickhunk 1 "o" "aa"
  , a2 <- fromAnonymousPrim $ quickhunk 2 "a34" "2xx"
  , og <- fromAnonymousPrim $ quickhunk 3 "4" "g"
  , ob <- fromAnonymousPrim $ quickhunk 1 "o" "bb"
  , b2 <- fromAnonymousPrim $ quickhunk 2 "b" "2"
  , oc <- fromAnonymousPrim $ quickhunk 1 "o" "cc"
  , od <- fromAnonymousPrim $ quickhunk 7 "x" "d"
  , oe <- fromAnonymousPrim $ quickhunk 7 "x" "e"
  , pf <- fromAnonymousPrim $ quickhunk 7 "x" "f"
  , od'' <- fromAnonymousPrim $ quickhunk 8 "x" "d"
  , ob' :>: b2' :>: _ :/\: _ <- mergeFL (oa :\/: ob :>: b2 :>: nilFL)
  , a2' :/\: _ <- merge (ob' :\/: a2)
  , ob'' :/\: _ <- merge (a2 :\/: ob')
  , og' :/\: _ <- merge (oa :\/: og)
  , og'' :/\: _ <- merge (a2 :\/: og')
  , og''' :/\: _ <- merge (ob' :\/: og')
  , oc' :/\: _ <- merge (oa :\/: oc)
  , oc'' :/\: _ <- merge (a2 :\/: oc)
  , oc''' :/\: _ <- merge (ob' :\/: oc')
  , oe' :/\: _ <- merge (od :\/: oe)
  , of' :/\: _ <- merge (od :\/: pf) =
    map
      (\(x :\/: y) -> Sealed2 (fromAnonymousPrim x :\/: fromAnonymousPrim y))
      mergeables ++
    repov2IglooMergeables ++
    repov2QuickcheckMergeables ++
    mergeExamples ++
    catMaybes (map pair2m (getPairs repov2FL)) ++
    [ Sealed2 (oa :\/: od)
    , Sealed2 (oa :\/: unsafeCoercePStart a2')
    , Sealed2 (ob' :\/: od'')
    , Sealed2 (oe :\/: od)
    , Sealed2 (of' :\/: oe')
    , Sealed2 (ob' :\/: oe')
    , Sealed2 (oa :\/: oe')
    , Sealed2 (ob' :\/: oc')
    , Sealed2 (b2' :\/: oc''')
    , Sealed2 (ob' :\/: a2)
    , Sealed2 (b2' :\/: og''')
    , Sealed2 (oc''' :\/: og''')
    , Sealed2 (oc'' :\/: og'')
    , Sealed2 (ob'' :\/: og'')
    , Sealed2 (ob'' :\/: oc'')
    , Sealed2 (oc' :\/: od'')
    ]
  | otherwise = error "impossible"

repov2IglooMergeables :: [Sealed2 (Patch :\/: Patch)]
repov2IglooMergeables
  | a <- fromAnonymousPrim $ quickhunk 1 "1" "A"
  , b <- fromAnonymousPrim $ quickhunk 2 "2" "B"
  , c <- fromAnonymousPrim $ quickhunk 3 "3" "C"
  , x <- fromAnonymousPrim $ quickhunk 1 "1BC" "xbc"
  , y <- fromAnonymousPrim $ quickhunk 1 "A2C" "ayc"
  , z <- fromAnonymousPrim $ quickhunk 1 "AB3" "abz"
  , x' :/\: _ <- merge (a :\/: x)
  , y' :/\: _ <- merge (b :\/: y)
  , z' :/\: _ <- merge (c :\/: z) =
    [ Sealed2 (a :\/: b)
    , Sealed2 (b :\/: c)
    , Sealed2 (a :\/: c)
    , Sealed2 (x :\/: a)
    , Sealed2 (y :\/: b)
    , Sealed2 (z :\/: c)
    , Sealed2 (x' :\/: y')
    , Sealed2 (z' :\/: y')
    , Sealed2 (x' :\/: z')
    , Sealed2 (a :\/: a)
    ]

repov2QuickcheckMergeables :: [Sealed2 (Patch :\/: Patch)]
repov2QuickcheckMergeables
  | hb <- fromAnonymousPrim $ quickhunk 0 "" "hb"
  , k <- fromAnonymousPrim $ quickhunk 0 "" "k"
  , n <- fromAnonymousPrim $ quickhunk 0 "" "n"
  , b <- fromAnonymousPrim $ quickhunk 1 "b" ""
  , d <- fromAnonymousPrim $ quickhunk 2 "" "d"
  , d' :/\: _ <- merge (b :\/: d)
  -- , k1 :>: n1 :>: _ :/\: _ <- mergeFL (hb :\/: k :>: n :>: nilFL)
  -- , k2 :>: n2 :>: _ :/\: _ <- merge (hb :>: b :>: nilFL :\/: k :>: n :>: nilFL)
  , k' :>: n' :>: _ :/\: _ :>: b' :>: _ <-
      merge (hb :>: b :>: d' :>: nilFL :\/: k :>: n :>: nilFL)
  , i <- fromAnonymousPrim $ quickhunk 0 "" "i"
  , x <- fromAnonymousPrim $ quickhunk 0 "" "x"
  , xi <- fromAnonymousPrim $ quickhunk 0 "xi" ""
  , d3 :/\: _ <- merge (xi :\/: d)
  , _ :/\: k3 <- mergeFL (k :\/: i :>: x :>: xi :>: d3 :>: nilFL) =
    -- Merging inverted RepoPatchV2 is no longer supported:
    -- [Sealed2 (invert k1 :\/: n1), Sealed2 (invert k2 :\/: n2)] ++
    [ Sealed2 (hb :\/: k)
    , Sealed2 (b' :\/: b')
    , Sealed2 (n' :\/: n')
    , Sealed2 (b :\/: d)
    , Sealed2 (k' :\/: k')
    , Sealed2 (k3 :\/: k3)
    ] ++ catMaybes (map pair2m (getPairs (hb :>: b :>: d' :>: k' :>: n' :>: nilFL)))
  | otherwise = error "impossible"

pair2m :: Sealed2 (Patch :> Patch) -> Maybe (Sealed2 (Patch :\/: Patch))
pair2m (Sealed2 (xx :> y)) = do
  y' :> _ <- commute (xx :> y)
  return $ Sealed2 (xx :\/: y')

nilFL :: FL Patch wX wX
nilFL = NilFL
