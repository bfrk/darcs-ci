#!/usr/bin/env bash

. lib

# so we always record non-canonized patches
pwd="$PWD"
trap "cp $pwd/defaults $pwd/.darcs/" EXIT
cp .darcs/defaults .
echo ALL no-canonize >> .darcs/defaults

# Expected output from mark-conflicts.
# Two versions because V1 and V2 don't sort the alternatives.
cat >log1 <<EOF
Cannot mark these conflicting patches:
replace ./b [A-Za-z_0-9] Y w
versus
replace ./b [A-Za-z_0-9] Y v
No conflicts to mark.
EOF

cat >log2 <<EOF
Cannot mark these conflicting patches:
replace ./b [A-Za-z_0-9] Y v
versus
replace ./b [A-Za-z_0-9] Y w
No conflicts to mark.
EOF

rm -rf B
darcs init B
cd B
mkdir a
touch b
darcs record -lam 'initial state'
cd ..

rm -rf S
darcs clone B S
cd S
darcs move ./b ./a/AJg.txt
darcs record -am mjyujvdcahsdqxgwzpda
cat >./a/AJg.txt <<EOF
I u
G
Y
EOF
echo y | darcs amend -a -p mjyujvdcahsdqxgwzpda

darcs replace Y v ./a/AJg.txt
darcs record -am jeyfglcxqipagormgcia
cd ..

rm -rf R1
darcs clone B R1
cd R1
rmdir ./a
darcs record -am ndatodkajykujxqtjpwp
darcs move ./b ./uL.txt
echo y | darcs amend -a -p ndatodkajykujxqtjpwp

darcs replace Y w ./uL.txt
# also depend on ndatodkajykujxqtjpwp explicitly
echo yd | darcs record -am dcncilcydqhpujrnvksn --ask-deps

darcs pull ../S -a --allow-conflicts -p mjyujvdcahsdqxgwzpda
darcs pull ../S -a --allow-conflicts -p jeyfglcxqipagormgcia

# # depend 4e0ec50916bbbd4dc9210d21e75bf412b2e49ddf
# #   * mjyujvdcahsdqxgwzpda
# # depend 7c521ce7eed39691b2e0d4a355859bd5df787e9f
# #   * ndatodkajykujxqtjpwp
# # depend 2da3103d6832f2a134c704202f916ad28e5925f2
# #   * dcncilcydqhpujrnvksn
# This actually does NOT depend on ndatodkajykujxqtjpwp:
# echo nyyyd | darcs record -a --ask-deps -m sqgzmtwutgroespcnhcc

# Emulate the single sqgzmtwutgroespcnhcc above that we can't realize here
# because of limitations when selecting patches to depend on.

# depend on ndatodkajykujxqtjpwp
echo nnnyd | darcs record -a --ask-deps -m intermediate
# depend on mjyujvdcahsdqxgwzpda, dcncilcydqhpujrnvksn, and intermediate
# (and therefore ndatodkajykujxqtjpwp)
echo ynyyd | darcs record -a --ask-deps -m sqgzmtwutgroespcnhcc

darcs mark-conflicts >log 2>&1
(diff -u ../log1 log || diff -u ../log2 log) >&2
cd ..

rm -rf R2
darcs init R2
cd R2
darcs pull ../R1 -a --allow-conflicts -p jeyfglcxqipagormgcia
darcs pull ../R1 -a --allow-conflicts -p ndatodkajykujxqtjpwp
darcs pull ../R1 -a --allow-conflicts -p dcncilcydqhpujrnvksn
darcs pull ../R1 -a --allow-conflicts -p mjyujvdcahsdqxgwzpda
darcs pull ../R1 -a --allow-conflicts -p intermediate
darcs pull ../R1 -a --allow-conflicts -p sqgzmtwutgroespcnhcc
darcs mark-conflicts >log 2>&1
(diff -u ../log1 log || diff -u ../log2 log) >&2
cd ..

exit # success

# Beautified complete output of the failing QC test case

Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 1383 tests and 9 shrinks):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (TokReplace "A-Za-z_0-9" "Y" "v")
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (TokReplace "A-Za-z_0-9" "Y" "w")
         } :>:
         NilFL)
  ]
]

r2=

[ [ Sealed
      (Prim
         { unPrim =
             Move
               (AnchoredPath [ Name { unName = "b" } ])
               (AnchoredPath
                  [ Name { unName = "a" } , Name { unName = "AJg.txt" } ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             Move
               (AnchoredPath [ Name { unName = "b" } ])
               (AnchoredPath [ Name { unName = "uL.txt" } ])
         } :>:
         NilFL)
  ]
, [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (TokReplace "A-Za-z_0-9" "Y" "v")
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (TokReplace "A-Za-z_0-9" "Y" "w")
         } :>:
         NilFL)
  ]
]

for patches

patch 7c521ce7eed39691b2e0d4a355859bd5df787e9f
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * ndatodkajykujxqtjpwp
rmdir ./a
move ./b ./uL.txt
patch 2da3103d6832f2a134c704202f916ad28e5925f2
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * dcncilcydqhpujrnvksn
depend 7c521ce7eed39691b2e0d4a355859bd5df787e9f
  * ndatodkajykujxqtjpwp
replace ./uL.txt [A-Za-z_0-9] Y w
patch 4e0ec50916bbbd4dc9210d21e75bf412b2e49ddf
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * mjyujvdcahsdqxgwzpda
conflictor
hash -81 ad3167ee294f86b047b7e942d9adf89bb16974bc
move ./uL.txt ./b
hash -57 eb0113cea8cd48d4f0708b74434ba659f885c9a3
adddir ./a
v v v v v v v
hash 57 eb0113cea8cd48d4f0708b74434ba659f885c9a3
rmdir ./a
*************
hash 81 ad3167ee294f86b047b7e942d9adf89bb16974bc
move ./b ./uL.txt
*************
hash 15 c7230c21b7cb710a96919dce19093e46e7cbf956
move ./b ./a/AJg.txt
^ ^ ^ ^ ^ ^ ^
hunk ./b 1
+I u
+G
+w
patch baaf5a5badc48ad6f447ef4a498dc9efaef09a17
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * jeyfglcxqipagormgcia
conflictor
hash -67 ab063243f8f27665186aaf9ffccfc36eb32043a7
replace ./b [A-Za-z_0-9] w Y
v v v v v v v
hash 67 ab063243f8f27665186aaf9ffccfc36eb32043a7
replace ./b [A-Za-z_0-9] Y w
*************
hash 27 7a19bc7ee8e9ba40c584a3353170e0c5fac64a5d
replace ./b [A-Za-z_0-9] Y v
^ ^ ^ ^ ^ ^ ^
patch 24047e8a378e356f88cdf97a2440bab8b77cc912
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * sqgzmtwutgroespcnhcc
depend 4e0ec50916bbbd4dc9210d21e75bf412b2e49ddf
  * mjyujvdcahsdqxgwzpda
depend 7c521ce7eed39691b2e0d4a355859bd5df787e9f
  * ndatodkajykujxqtjpwp
depend 2da3103d6832f2a134c704202f916ad28e5925f2
  * dcncilcydqhpujrnvksn

versus

patch baaf5a5badc48ad6f447ef4a498dc9efaef09a17
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * jeyfglcxqipagormgcia
replace ./b [A-Za-z_0-9] Y v
patch 7c521ce7eed39691b2e0d4a355859bd5df787e9f
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * ndatodkajykujxqtjpwp
rmdir ./a
move ./b ./uL.txt
patch 2da3103d6832f2a134c704202f916ad28e5925f2
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * dcncilcydqhpujrnvksn
depend 7c521ce7eed39691b2e0d4a355859bd5df787e9f
  * ndatodkajykujxqtjpwp
conflictor
hash -27 7a19bc7ee8e9ba40c584a3353170e0c5fac64a5d
replace ./uL.txt [A-Za-z_0-9] v Y
v v v v v v v
hash 27 7a19bc7ee8e9ba40c584a3353170e0c5fac64a5d
replace ./uL.txt [A-Za-z_0-9] Y v
*************
hash 67 ab063243f8f27665186aaf9ffccfc36eb32043a7
replace ./uL.txt [A-Za-z_0-9] Y w
^ ^ ^ ^ ^ ^ ^
patch 4e0ec50916bbbd4dc9210d21e75bf412b2e49ddf
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * mjyujvdcahsdqxgwzpda
conflictor
hash -81 ad3167ee294f86b047b7e942d9adf89bb16974bc
move ./uL.txt ./b
hash -57 eb0113cea8cd48d4f0708b74434ba659f885c9a3
adddir ./a
v v v v v v v
hash 57 eb0113cea8cd48d4f0708b74434ba659f885c9a3
rmdir ./a
*************
hash 81 ad3167ee294f86b047b7e942d9adf89bb16974bc
move ./b ./uL.txt
*************
hash 15 c7230c21b7cb710a96919dce19093e46e7cbf956
move ./b ./a/AJg.txt
^ ^ ^ ^ ^ ^ ^
hunk ./b 1
+I u
+G
+Y
patch 24047e8a378e356f88cdf97a2440bab8b77cc912
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * sqgzmtwutgroespcnhcc
depend 4e0ec50916bbbd4dc9210d21e75bf412b2e49ddf
  * mjyujvdcahsdqxgwzpda
depend 7c521ce7eed39691b2e0d4a355859bd5df787e9f
  * ndatodkajykujxqtjpwp
depend 2da3103d6832f2a134c704202f916ad28e5925f2
  * dcncilcydqhpujrnvksn

Sealed2
  (WithStartState2
     (WithNames V1Model [ Dir "a" , File "b" [] ] [])
     (SeqMS
        (ParMS
           (SeqMS
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "mjyujvdcahsdqxgwzpda"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 15 c7230c21b7cb710a96919dce19093e46e7cbf956)
                       Prim
                         { unPrim =
                             Move
                               (AnchoredPath [ Name { unName = "b" } ])
                               (AnchoredPath
                                  [ Name { unName = "a" } , Name { unName = "AJg.txt" } ])
                         } :>:
                       (PrimWithName
                          (PrimPatchId 63 09627d09eaf29c8a349ac2ad4a1d08e951a1c095)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath
                                     [ Name { unName = "a" } , Name { unName = "AJg.txt" } ])
                                  (Hunk 1 [] [ "I u" , "G" , "Y" ])
                            } :>:
                          NilFL))))
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "jeyfglcxqipagormgcia"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 []
                 (PrimWithName
                    (PrimPatchId 27 7a19bc7ee8e9ba40c584a3353170e0c5fac64a5d)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath
                               [ Name { unName = "a" } , Name { unName = "AJg.txt" } ])
                            (TokReplace "A-Za-z_0-9" "Y" "v")
                      } :>:
                    NilFL)))
           (SeqMS
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "ndatodkajykujxqtjpwp"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 57 eb0113cea8cd48d4f0708b74434ba659f885c9a3)
                       Prim
                         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
                       (PrimWithName
                          (PrimPatchId 81 ad3167ee294f86b047b7e942d9adf89bb16974bc)
                          Prim
                            { unPrim =
                                Move
                                  (AnchoredPath [ Name { unName = "b" } ])
                                  (AnchoredPath [ Name { unName = "uL.txt" } ])
                            } :>:
                          NilFL))))
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "dcncilcydqhpujrnvksn"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 [ PatchInfo
                     { _piDate = "20240606010532"
                     , _piName = "ndatodkajykujxqtjpwp"
                     , _piAuthor = "tester"
                     , _piLog = []
                     , _piLegacyIsInverted = False
                     }
                 ]
                 (PrimWithName
                    (PrimPatchId 67 ab063243f8f27665186aaf9ffccfc36eb32043a7)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "uL.txt" } ])
                            (TokReplace "A-Za-z_0-9" "Y" "w")
                      } :>:
                    NilFL))))
        (NamedP
           PatchInfo
             { _piDate = "20240606010532"
             , _piName = "sqgzmtwutgroespcnhcc"
             , _piAuthor = "tester"
             , _piLog = []
             , _piLegacyIsInverted = False
             }
           [ PatchInfo
               { _piDate = "20240606010532"
               , _piName = "mjyujvdcahsdqxgwzpda"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           , PatchInfo
               { _piDate = "20240606010532"
               , _piName = "ndatodkajykujxqtjpwp"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           , PatchInfo
               { _piDate = "20240606010532"
               , _piName = "dcncilcydqhpujrnvksn"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           ]
           NilFL)))

(used seed 6479663611722759425)
