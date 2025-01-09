#!/usr/bin/env bash

. lib

rm -rf B
darcs init B
cd B
cat >a <<EOF
f
EOF
darcs record -lam 'initial state'
cd ..

# irrelevant, not used
rm -rf B1
darcs clone B B1
cd B1
# empty patch
touch x
darcs record -lam gepgjtanefpzuhyxmxcr
rm x
echo y | darcs amend -a -p gepgjtanefpzuhyxmxcr
cd ..

rm -rf B2
darcs clone B B2
cd B2
cat >a <<EOF
a
f R
EOF
darcs record -lam lgeoexmrdwvgtnlahuxs
cat >a <<EOF
U
EOF
echo y | darcs amend --no-canonize -am lgeoexmrdwvgtnlahuxs
cd ..

rm -rf B3
darcs clone B B3
cd B3
cat >a <<EOF
f
f F
EOF
darcs record -lam vlstfkqxkukjydejkqgt
darcs replace f E a
echo y | darcs amend --no-canonize -am vlstfkqxkukjydejkqgt
cd ..

rm -rf B4
darcs clone B B4
cd B4
darcs move a T.txt
darcs record -lam lizpqmmnboazzdnlbnxf
cat >T.txt <<EOF
U
d
R A
X
f
EOF
echo y | darcs amend --no-canonize -am lizpqmmnboazzdnlbnxf
cd ..

rm -rf B5
darcs clone B B5
cd B5
darcs replace f q a
darcs record -lam hgugmytjbqlawubbavvd
cd ..

rm -rf C
darcs clone B C
cd C
#darcs pull -a --allow-conflicts ../B1 -p gepgjtanefpzuhyxmxcr
darcs pull -a --allow-conflicts ../B2 ../B3 ../B4 ../B5
# depend on lgeoexmrdwvgtnlahuxs
# depend on vlstfkqxkukjydejkqgt
# depend on hgugmytjbqlawubbavvd
echo ynyyd | darcs record --ask-deps -am wkotffbwxvocllnnvdvp
cd ..

rm -rf R1
darcs clone B R1
cd R1
# context
darcs pull -a --allow-conflicts ../C -p vlstfkqxkukjydejkqgt
darcs pull -a --allow-conflicts ../C -p lgeoexmrdwvgtnlahuxs
#darcs pull -a --allow-conflicts ../C -p gepgjtanefpzuhyxmxcr
# the next line crashes darcs with darcs-1 patches
darcs pull -a --allow-conflicts ../C -p lizpqmmnboazzdnlbnxf
# patches
#darcs pull -a --mark-conflicts ../C 2>&1 | grep -v Backing > log
darcs tag resolved
darcs pull -a --allow-conflicts ../C
darcs mark-conflicts >log 2>&1
not darcs whatsnew
cd ..

rm -rf R2
darcs clone B R2
cd R2
# context
#darcs pull -a --allow-conflicts ../C -p gepgjtanefpzuhyxmxcr
darcs pull -a --allow-conflicts ../C -p lgeoexmrdwvgtnlahuxs
darcs pull -a --allow-conflicts ../C -p lizpqmmnboazzdnlbnxf
darcs pull -a --allow-conflicts ../C -p vlstfkqxkukjydejkqgt
# patches
#darcs pull -a --mark-conflicts ../C 2>&1 | grep -v Backing > log
darcs pull ../R1 -a -t resolved
darcs pull -a --allow-conflicts ../C
darcs mark-conflicts >log 2>&1
not darcs whatsnew
cd ..

diff -u R1/log R2/log >&2

exit

Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 64289 tests and 11 shrinks):
resolutions differ: r1=

[]

r2=

[ [ Sealed
      -- vlstfkqxkukjydejkqgt 2
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "T.txt" } ])
               (TokReplace "A-Za-z_0-9" "f" "q")
         } :>:
         NilFL)
  , Sealed
      -- hgugmytjbqlawubbavvd
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "T.txt" } ])
               (TokReplace "A-Za-z_0-9" "f" "E")
         } :>:
         NilFL)
  ]
]

for context

patch 28835563346ea5df54afe26c435563f2d57a4d71
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * vlstfkqxkukjydejkqgt
hash 58 bb9908351ba91c082b051d1e6eb820a742d97fc6
hunk ./a 2
+f F
hash 68 196c27fbda15e4f995a60e88a090f249e02c8f15
replace ./a [A-Za-z_0-9] f E
patch 39fed960a889a4bf748c2722af9982dde0d31488
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * lgeoexmrdwvgtnlahuxs
conflictor
hash -58 bb9908351ba91c082b051d1e6eb820a742d97fc6
hunk ./a 2
-E F
v v v v v v v
hash 58 bb9908351ba91c082b051d1e6eb820a742d97fc6
hunk ./a 2
+E F
*************
hash 79 6c584e1373537e7ef3dba3a27f9f78ae9be92b42
hunk ./a 1
-E
+a
+E R
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 58 bb9908351ba91c082b051d1e6eb820a742d97fc6
hunk ./a 2
+E F
*************
hash 53 9a12eddf3317e62d87bbae7a31fb17146d50475d
hunk ./a 1
-E
+U
^ ^ ^ ^ ^ ^ ^
patch d158c6b4cd13498a4c0af74150dff4f08be7e540
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * gepgjtanefpzuhyxmxcr
patch 54680f79d948878ce7a2407389af93bec5f37a51
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * lizpqmmnboazzdnlbnxf
hash 44 758a11bf60f90707efc5092cf8dca0c220b5253b
move ./a ./T.txt
conflictor
v v v v v v v
hash 53 9a12eddf3317e62d87bbae7a31fb17146d50475d
hunk ./T.txt 1
-E
+U
*************
hash 79 6c584e1373537e7ef3dba3a27f9f78ae9be92b42
hunk ./T.txt 1
-E
+a
+E R
*************
hash 33 6f3797b497b06a9addc64552a8a0a557af4dac4b
hunk ./T.txt 1
+U
+d
+R A
+X
^ ^ ^ ^ ^ ^ ^

and patches

patch fc493018bee29a7830033ac7b84459beea51696d
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * hgugmytjbqlawubbavvd
conflictor
hash -68 196c27fbda15e4f995a60e88a090f249e02c8f15
replace ./T.txt [A-Za-z_0-9] E f
v v v v v v v
hash 68 196c27fbda15e4f995a60e88a090f249e02c8f15
replace ./T.txt [A-Za-z_0-9] f E
*************
hash 61 ab53b2276a54cbbd20e268ec12dd8b0ec02df27c
replace ./T.txt [A-Za-z_0-9] f q
^ ^ ^ ^ ^ ^ ^
patch 04d74fdab4eb33b56a779a140f1995b01abee292
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * wkotffbwxvocllnnvdvp
depend 39fed960a889a4bf748c2722af9982dde0d31488
  * lgeoexmrdwvgtnlahuxs
depend 28835563346ea5df54afe26c435563f2d57a4d71
  * vlstfkqxkukjydejkqgt
depend fc493018bee29a7830033ac7b84459beea51696d
  * hgugmytjbqlawubbavvd

versus

for context

patch d158c6b4cd13498a4c0af74150dff4f08be7e540
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * gepgjtanefpzuhyxmxcr
patch 39fed960a889a4bf748c2722af9982dde0d31488
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * lgeoexmrdwvgtnlahuxs
hash 79 6c584e1373537e7ef3dba3a27f9f78ae9be92b42
hunk ./a 1
-f
+a
+f R
hash 53 9a12eddf3317e62d87bbae7a31fb17146d50475d
hunk ./a 1
-a
-f R
+U
patch 54680f79d948878ce7a2407389af93bec5f37a51
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * lizpqmmnboazzdnlbnxf
hash 44 758a11bf60f90707efc5092cf8dca0c220b5253b
move ./a ./T.txt
conflictor
hash -53 9a12eddf3317e62d87bbae7a31fb17146d50475d
hunk ./T.txt 1
-U
+a
+f R
hash -79 6c584e1373537e7ef3dba3a27f9f78ae9be92b42
hunk ./T.txt 1
-a
-f R
+f
v v v v v v v
hash 53 9a12eddf3317e62d87bbae7a31fb17146d50475d
hunk ./T.txt 1
-f
+U
*************
hash 79 6c584e1373537e7ef3dba3a27f9f78ae9be92b42
hunk ./T.txt 1
-f
+a
+f R
*************
hash 33 6f3797b497b06a9addc64552a8a0a557af4dac4b
hunk ./T.txt 1
+U
+d
+R A
+X
^ ^ ^ ^ ^ ^ ^
patch 28835563346ea5df54afe26c435563f2d57a4d71
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * vlstfkqxkukjydejkqgt
conflictor
v v v v v v v
hash 53 9a12eddf3317e62d87bbae7a31fb17146d50475d
hunk ./T.txt 1
-f
+U
*************
hash 79 6c584e1373537e7ef3dba3a27f9f78ae9be92b42
hunk ./T.txt 1
-f
+a
+f R
*************
hash 58 bb9908351ba91c082b051d1e6eb820a742d97fc6
hunk ./T.txt 2
+f F
^ ^ ^ ^ ^ ^ ^
hash 68 196c27fbda15e4f995a60e88a090f249e02c8f15
replace ./T.txt [A-Za-z_0-9] f E

and patches

patch fc493018bee29a7830033ac7b84459beea51696d
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * hgugmytjbqlawubbavvd
conflictor
hash -68 196c27fbda15e4f995a60e88a090f249e02c8f15
replace ./T.txt [A-Za-z_0-9] E f
v v v v v v v
hash 68 196c27fbda15e4f995a60e88a090f249e02c8f15
replace ./T.txt [A-Za-z_0-9] f E
*************
hash 61 ab53b2276a54cbbd20e268ec12dd8b0ec02df27c
replace ./T.txt [A-Za-z_0-9] f q
^ ^ ^ ^ ^ ^ ^
patch 04d74fdab4eb33b56a779a140f1995b01abee292
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * wkotffbwxvocllnnvdvp
depend 39fed960a889a4bf748c2722af9982dde0d31488
  * lgeoexmrdwvgtnlahuxs
depend 28835563346ea5df54afe26c435563f2d57a4d71
  * vlstfkqxkukjydejkqgt
depend fc493018bee29a7830033ac7b84459beea51696d
  * hgugmytjbqlawubbavvd

Sealed2
  (WithStartState2
     (WithNames V1Model [ File "a" [ "f" ] ] [])
     (WithSplit
        2
        (SeqMS
           (ParMS
              (ParMS
                 (ParMS
                    (SeqMS
                       NilMS
                       (NamedP
                          PatchInfo
                            { _piDate = "20240606010532"
                            , _piName = "gepgjtanefpzuhyxmxcr"
                            , _piAuthor = "tester"
                            , _piLog = []
                            , _piLegacyIsInverted = False
                            }
                          []
                          NilFL))
                    (SeqMS
                       NilMS
                       (NamedP
                          PatchInfo
                            { _piDate = "20240606010532"
                            , _piName = "lgeoexmrdwvgtnlahuxs"
                            , _piAuthor = "tester"
                            , _piLog = []
                            , _piLegacyIsInverted = False
                            }
                          []
                          (PrimWithName
                             (PrimPatchId 79 6c584e1373537e7ef3dba3a27f9f78ae9be92b42)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "a" } ])
                                     (Hunk 1 [ "f" ] [ "a" , "f R" ])
                               } :>:
                             (PrimWithName
                                (PrimPatchId 53 9a12eddf3317e62d87bbae7a31fb17146d50475d)
                                Prim
                                  { unPrim =
                                      FP
                                        (AnchoredPath [ Name { unName = "a" } ])
                                        (Hunk 1 [ "a" , "f R" ] [ "U" ])
                                  } :>:
                                NilFL)))))
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "vlstfkqxkukjydejkqgt"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 58 bb9908351ba91c082b051d1e6eb820a742d97fc6)
                          Prim
                            { unPrim =
                                FP (AnchoredPath [ Name { unName = "a" } ]) (Hunk 2 [] [ "f F" ])
                            } :>:
                          (PrimWithName
                             (PrimPatchId 68 196c27fbda15e4f995a60e88a090f249e02c8f15)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "a" } ])
                                     (TokReplace "A-Za-z_0-9" "f" "E")
                               } :>:
                             NilFL)))))
              (ParMS
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "lizpqmmnboazzdnlbnxf"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 44 758a11bf60f90707efc5092cf8dca0c220b5253b)
                          Prim
                            { unPrim =
                                Move
                                  (AnchoredPath [ Name { unName = "a" } ])
                                  (AnchoredPath [ Name { unName = "T.txt" } ])
                            } :>:
                          (PrimWithName
                             (PrimPatchId 33 6f3797b497b06a9addc64552a8a0a557af4dac4b)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "T.txt" } ])
                                     (Hunk 1 [] [ "U" , "d" , "R A" , "X" ])
                               } :>:
                             NilFL))))
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "hgugmytjbqlawubbavvd"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 61 ab53b2276a54cbbd20e268ec12dd8b0ec02df27c)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "a" } ])
                                  (TokReplace "A-Za-z_0-9" "f" "q")
                            } :>:
                          NilFL)))))
           (NamedP
              PatchInfo
                { _piDate = "20240606010532"
                , _piName = "wkotffbwxvocllnnvdvp"
                , _piAuthor = "tester"
                , _piLog = []
                , _piLegacyIsInverted = False
                }
              [ PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "lgeoexmrdwvgtnlahuxs"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              , PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "vlstfkqxkukjydejkqgt"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              , PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "hgugmytjbqlawubbavvd"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              ]
              NilFL))))

(used seed 1083374694840011397) -q=100000
