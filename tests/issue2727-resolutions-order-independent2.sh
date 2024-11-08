#!/usr/bin/env bash

# Slightly simplified manual reconstruction of:
# Named RepoPatchV3:
#   using V2.Prim wrapper for Prim.V1:
#     resolutions are invariant under reorderings: [Failed]
# *** Failed! (after 99 tests and 40 shrinks):
# [...]
# (used seed -7870230807607679712)

. lib

# expected output for all reorderings
cat >log <<EOF
No conflicts to mark.
EOF

rm -rf R1
darcs init R1
cd R1
touch a
darcs record -lam 'initial state'

rm -rf ../S
darcs clone . ../S

darcs move a R.txt
darcs record -am xbkikpxxbyoohohyxnvo

cd ../S

cat >a <<EOF
V Y
J W
J
EOF
darcs move a g.txt
darcs record -am gbpkzeypshrtfirwnvhl

cd ../R1

darcs pull -a --allow-conflicts ../S

# depend on xbkikpxxbyoohohyxnvo
echo nyd | darcs record --ask-deps -m oqdpurqjnggqiznmdifn

# irrelevant extra patch
# mkdir BNC
# darcs add BNC
# darcs move BNC V
# darcs record -am rbokwwdfgukkjukzunjg

# depend on gbpkzeypshrtfirwnvhl and xbkikpxxbyoohohyxnvo
echo nyyd | darcs record --ask-deps -m argiyzgbxqgskixomnuu
darcs mark-conflicts >log 2>&1
diff -u ../log log >&2
cd ..

rm -rf R2
darcs init R2
cd R2
darcs pull -a ../R1 -p oqdpurqjnggqiznmdifn
darcs pull -a --allow-conflicts ../R1
darcs mark-conflicts >log 2>&1
diff -u ../log log >&2
cd ..

exit # success

# Beautified complete output of the failing QC test case

Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 99 tests and 40 shrinks):
resolutions differ: r1=

[]

r2=

[ [ Sealed
      (Prim
         { unPrim =
             Move
               (AnchoredPath [ Name { unName = "a" } ])
               (AnchoredPath [ Name { unName = "R.txt" } ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             Move
               (AnchoredPath [ Name { unName = "a" } ])
               (AnchoredPath [ Name { unName = "g.txt" } ])
         } :>:
         NilFL)
  ]
]

for patches

patch 8d0f0563f8834e222234f837f4b1bb93c377d3b8
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * xbkikpxxbyoohohyxnvo
move ./a ./R.txt
patch 815947a05dc371855fbaadd16235247d0c794c91
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * gbpkzeypshrtfirwnvhl
hunk ./R.txt 1
+V Y
+J W
+J
conflictor
hash -17 445e2df79b6442cdc97ecff12c439e5bdd7a7aea
move ./R.txt ./a
v v v v v v v
hash 17 445e2df79b6442cdc97ecff12c439e5bdd7a7aea
move ./a ./R.txt
*************
hash 69 6d10ffc887dcb55b698728e78460525cd9896c39
move ./a ./g.txt
^ ^ ^ ^ ^ ^ ^
patch a9f36328f99a8a31a178f6fda4a8b4ddf8df6f96
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * oqdpurqjnggqiznmdifn
depend 8d0f0563f8834e222234f837f4b1bb93c377d3b8
  * xbkikpxxbyoohohyxnvo
patch b02015183418696b10c22a57603b004dff50a7ec
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * rbokwwdfgukkjukzunjg
adddir ./BNC
move ./BNC ./V
patch db5de13b37d37bc562426c17ef930afd688982a9
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * argiyzgbxqgskixomnuu
depend 815947a05dc371855fbaadd16235247d0c794c91
  * gbpkzeypshrtfirwnvhl
depend 8d0f0563f8834e222234f837f4b1bb93c377d3b8
  * xbkikpxxbyoohohyxnvo

versus

patch 8d0f0563f8834e222234f837f4b1bb93c377d3b8
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * xbkikpxxbyoohohyxnvo
move ./a ./R.txt
patch a9f36328f99a8a31a178f6fda4a8b4ddf8df6f96
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * oqdpurqjnggqiznmdifn
depend 8d0f0563f8834e222234f837f4b1bb93c377d3b8
  * xbkikpxxbyoohohyxnvo
patch 815947a05dc371855fbaadd16235247d0c794c91
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * gbpkzeypshrtfirwnvhl
hunk ./R.txt 1
+V Y
+J W
+J
conflictor
hash -17 445e2df79b6442cdc97ecff12c439e5bdd7a7aea
move ./R.txt ./a
v v v v v v v
hash 17 445e2df79b6442cdc97ecff12c439e5bdd7a7aea
move ./a ./R.txt
*************
hash 69 6d10ffc887dcb55b698728e78460525cd9896c39
move ./a ./g.txt
^ ^ ^ ^ ^ ^ ^
patch b02015183418696b10c22a57603b004dff50a7ec
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * rbokwwdfgukkjukzunjg
adddir ./BNC
move ./BNC ./V
patch db5de13b37d37bc562426c17ef930afd688982a9
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * argiyzgbxqgskixomnuu
depend 815947a05dc371855fbaadd16235247d0c794c91
  * gbpkzeypshrtfirwnvhl
depend 8d0f0563f8834e222234f837f4b1bb93c377d3b8
  * xbkikpxxbyoohohyxnvo

Sealed2
  (WithStartState2
     (WithNames V1Model [ File "a" [] ] [])
     (SeqMS
        (SeqMS
           (ParMS
              (ParMS
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "gbpkzeypshrtfirwnvhl"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 62 5d5373b4231aba09cea3b47754b5f5ece2cfe5a9)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "a" } ])
                                  (Hunk 1 [] [ "V Y" , "J W" , "J" ])
                            } :>:
                          (PrimWithName
                             (PrimPatchId 69 6d10ffc887dcb55b698728e78460525cd9896c39)
                             Prim
                               { unPrim =
                                   Move
                                     (AnchoredPath [ Name { unName = "a" } ])
                                     (AnchoredPath [ Name { unName = "g.txt" } ])
                               } :>:
                             NilFL))))
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "rbokwwdfgukkjukzunjg"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 21 2ceccc937280317d079b556c14fabeb372a263be)
                          Prim
                            { unPrim = DP (AnchoredPath [ Name { unName = "BNC" } ]) AddDir
                            } :>:
                          (PrimWithName
                             (PrimPatchId 63 cee1499945193e1b5b260a3313f217508b175b82)
                             Prim
                               { unPrim =
                                   Move
                                     (AnchoredPath [ Name { unName = "BNC" } ])
                                     (AnchoredPath [ Name { unName = "V" } ])
                               } :>:
                             NilFL)))))
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "xbkikpxxbyoohohyxnvo"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 17 445e2df79b6442cdc97ecff12c439e5bdd7a7aea)
                       Prim
                         { unPrim =
                             Move
                               (AnchoredPath [ Name { unName = "a" } ])
                               (AnchoredPath [ Name { unName = "R.txt" } ])
                         } :>:
                       NilFL))))
           (NamedP
              PatchInfo
                { _piDate = "20240606010532"
                , _piName = "argiyzgbxqgskixomnuu"
                , _piAuthor = "tester"
                , _piLog = []
                , _piLegacyIsInverted = False
                }
              [ PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "gbpkzeypshrtfirwnvhl"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              , PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "xbkikpxxbyoohohyxnvo"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              ]
              NilFL))
        (NamedP
           PatchInfo
             { _piDate = "20240606010532"
             , _piName = "oqdpurqjnggqiznmdifn"
             , _piAuthor = "tester"
             , _piLog = []
             , _piLegacyIsInverted = False
             }
           [ PatchInfo
               { _piDate = "20240606010532"
               , _piName = "xbkikpxxbyoohohyxnvo"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           ]
           NilFL)))

(used seed -7870230807607679712)
