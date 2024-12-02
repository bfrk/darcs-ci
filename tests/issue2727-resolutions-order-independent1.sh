#!/usr/bin/env bash

# Manual reconstruction of:
# Named RepoPatchV3:
#   using V2.Prim wrapper for Prim.V1:
#     resolutions are invariant under reorderings: [Failed]
# *** Failed! (after 67 tests and 30 shrinks):
# [...]
# (used seed -580976364380586561)

. lib

# expected output for all reorderings; note that the conflict is fully
# (transitively) covered by the single patch xjdeyyjdyibnmfpszdrp,
# see comment below where we record it
cat >log <<EOF
No conflicts to mark.
EOF

rm -rf R1 S R2

darcs init R1
cd R1

mkdir ./a
# irrelevant extra prim:
touch ./b
darcs record -lam 'initial state'

darcs clone . ../S

touch ./a/HX.txt
# irrelevant extra prim:
cat >./b <<EOF
J m
b
z u
g
O
x
X L
V
d y
j L
u F
S
Q
x a
y
i
t C
W
p
EOF
darcs record -lam kwkmkvebzoazwwreddfi

cd ../S
rmdir ./a
darcs record -lam xzjgiiwvaanwuomlhnri
cd ../R1
darcs pull -a --allow-conflicts -p xzjgiiwvaanwuomlhnri ../S

# depend on kwkmkvebzoazwwreddfi
echo nyd | darcs record --ask-deps -m qgfgwkdlhlwkrlvqomlv

# # depend on xzjgiiwvaanwuomlhnri
# irrelevant extra patch:
echo nyd | darcs record --ask-deps -m wrszpgidbqmixqwgtkck

# depend on qgfgwkdlhlwkrlvqomlv (and thus indirectly kwkmkvebzoazwwreddfi)
# and on xzjgiiwvaanwuomlhnri, so the conflict is fully covered
echo nyyd | darcs record --ask-deps -m xjdeyyjdyibnmfpszdrp

darcs mark-conflicts >log 2>&1
diff -u ../log log >&2

cd ..

# same patches, different order
# (qgfgwkdlhlwkrlvqomlv comes before xzjgiiwvaanwuomlhnri)
darcs init R2
cd R2
darcs pull ../R1 -a -p qgfgwkdlhlwkrlvqomlv
# pull the rest
darcs pull ../R1 -a --allow-conflicts
darcs mark-conflicts >log 2>&1
diff -u ../log log >&2
cd ..

# more reorderings
rm -rf R3 R4
darcs init R3
darcs init R4
cd R3
darcs pull ../R1 -a -p xzjgiiwvaanwuomlhnri
darcs pull ../R1 -a --allow-conflicts -p kwkmkvebzoazwwreddfi
darcs pull ../R1 -a -p wrszpgidbqmixqwgtkck
darcs pull ../R1 -a -p qgfgwkdlhlwkrlvqomlv
darcs pull ../R1 -a -p xjdeyyjdyibnmfpszdrp
darcs mark-conflicts >log 2>&1
diff -u ../log log >&2
cd ..
cd R4
darcs pull ../R1 -a -p xzjgiiwvaanwuomlhnri
darcs pull ../R1 -a -p wrszpgidbqmixqwgtkck
darcs pull ../R1 -a --allow-conflicts -p kwkmkvebzoazwwreddfi
darcs pull ../R1 -a -p qgfgwkdlhlwkrlvqomlv
darcs pull ../R1 -a -p xjdeyyjdyibnmfpszdrp
darcs mark-conflicts >log 2>&1
diff -u ../log log >&2
cd ..

exit # success

# Beautified complete output of the failing QC test case

Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 67 tests and 30 shrinks):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath
                  [ Name { unName = "a" } , Name { unName = "HX.txt" } ])
               AddFile
         } :>:
         NilFL)
  ]
]

r2=

[]

for patches

patch 29cb9bb4b7ddddd6c2d29231af2a24b16d5f7dee
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * kwkmkvebzoazwwreddfi
addfile ./a/HX.txt
hunk ./b 1
+J m
+b
+z u
+g
+O
+x
+X L
+V
+d y
+j L
+u F
+S
+Q
+x a
+y
+i
+t C
+W
+p
patch bbd7551ea6a300453f31e183e557b8d64862af81
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * qgfgwkdlhlwkrlvqomlv
depend 29cb9bb4b7ddddd6c2d29231af2a24b16d5f7dee
  * kwkmkvebzoazwwreddfi
patch 8830f4d5e74044abe99b5293ff8d798d1deefc42
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * xzjgiiwvaanwuomlhnri
conflictor
hash -45 854e976bb830f9a93b65751d86b2a7116204bbee
rmfile ./a/HX.txt
v v v v v v v
hash 45 854e976bb830f9a93b65751d86b2a7116204bbee
addfile ./a/HX.txt
*************
hash 15 2d58936ad26e5da9e486252bf280b638d303b34e
rmdir ./a
^ ^ ^ ^ ^ ^ ^
patch 2aad7b0472f94fd65f83d37f7802348619251241
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * wrszpgidbqmixqwgtkck
depend 8830f4d5e74044abe99b5293ff8d798d1deefc42
  * xzjgiiwvaanwuomlhnri
patch 6aee31fac33af70f1a90395de0f6785fc91d8ac2
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * xjdeyyjdyibnmfpszdrp
depend bbd7551ea6a300453f31e183e557b8d64862af81
  * qgfgwkdlhlwkrlvqomlv
depend 8830f4d5e74044abe99b5293ff8d798d1deefc42
  * xzjgiiwvaanwuomlhnri

versus

patch 29cb9bb4b7ddddd6c2d29231af2a24b16d5f7dee
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * kwkmkvebzoazwwreddfi
addfile ./a/HX.txt
hunk ./b 1
+J m
+b
+z u
+g
+O
+x
+X L
+V
+d y
+j L
+u F
+S
+Q
+x a
+y
+i
+t C
+W
+p
patch 8830f4d5e74044abe99b5293ff8d798d1deefc42
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * xzjgiiwvaanwuomlhnri
conflictor
hash -45 854e976bb830f9a93b65751d86b2a7116204bbee
rmfile ./a/HX.txt
v v v v v v v
hash 45 854e976bb830f9a93b65751d86b2a7116204bbee
addfile ./a/HX.txt
*************
hash 15 2d58936ad26e5da9e486252bf280b638d303b34e
rmdir ./a
^ ^ ^ ^ ^ ^ ^
patch bbd7551ea6a300453f31e183e557b8d64862af81
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * qgfgwkdlhlwkrlvqomlv
depend 29cb9bb4b7ddddd6c2d29231af2a24b16d5f7dee
  * kwkmkvebzoazwwreddfi
patch 2aad7b0472f94fd65f83d37f7802348619251241
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * wrszpgidbqmixqwgtkck
depend 8830f4d5e74044abe99b5293ff8d798d1deefc42
  * xzjgiiwvaanwuomlhnri
patch 6aee31fac33af70f1a90395de0f6785fc91d8ac2
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * xjdeyyjdyibnmfpszdrp
depend bbd7551ea6a300453f31e183e557b8d64862af81
  * qgfgwkdlhlwkrlvqomlv
depend 8830f4d5e74044abe99b5293ff8d798d1deefc42
  * xzjgiiwvaanwuomlhnri

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
                      , _piName = "kwkmkvebzoazwwreddfi"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 45 854e976bb830f9a93b65751d86b2a7116204bbee)
                       Prim
                         { unPrim =
                             FP
                               (AnchoredPath
                                  [ Name { unName = "a" } , Name { unName = "HX.txt" } ])
                               AddFile
                         } :>:
                       (PrimWithName
                          (PrimPatchId 44 fa5470f2fb48c14457e1f14929bc7913f3487109)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "b" } ])
                                  (Hunk
                                     1
                                     []
                                     [ "J m"
                                     , "b"
                                     , "z u"
                                     , "g"
                                     , "O"
                                     , "x"
                                     , "X L"
                                     , "V"
                                     , "d y"
                                     , "j L"
                                     , "u F"
                                     , "S"
                                     , "Q"
                                     , "x a"
                                     , "y"
                                     , "i"
                                     , "t C"
                                     , "W"
                                     , "p"
                                     ])
                            } :>:
                          NilFL))))
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "qgfgwkdlhlwkrlvqomlv"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 [ PatchInfo
                     { _piDate = "20240606010532"
                     , _piName = "kwkmkvebzoazwwreddfi"
                     , _piAuthor = "tester"
                     , _piLog = []
                     , _piLegacyIsInverted = False
                     }
                 ]
                 NilFL))
           (SeqMS
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "xzjgiiwvaanwuomlhnri"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 15 2d58936ad26e5da9e486252bf280b638d303b34e)
                       Prim
                         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
                       NilFL)))
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "wrszpgidbqmixqwgtkck"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 [ PatchInfo
                     { _piDate = "20240606010532"
                     , _piName = "xzjgiiwvaanwuomlhnri"
                     , _piAuthor = "tester"
                     , _piLog = []
                     , _piLegacyIsInverted = False
                     }
                 ]
                 NilFL)))
        (NamedP
           PatchInfo
             { _piDate = "20240606010532"
             , _piName = "xjdeyyjdyibnmfpszdrp"
             , _piAuthor = "tester"
             , _piLog = []
             , _piLegacyIsInverted = False
             }
           [ PatchInfo
               { _piDate = "20240606010532"
               , _piName = "qgfgwkdlhlwkrlvqomlv"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           , PatchInfo
               { _piDate = "20240606010532"
               , _piName = "xzjgiiwvaanwuomlhnri"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           ]
           NilFL)))

(used seed -580976364380586561)
