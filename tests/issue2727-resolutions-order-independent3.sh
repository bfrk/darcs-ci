#!/usr/bin/env bash

# Manual reconstruction of
# Named RepoPatchV3:
#   using V2.Prim wrapper for Prim.V1:
#     resolutions are invariant under reorderings: [Failed]
# *** Failed! (after 2338 tests and 5 shrinks):
# (used seed 5663187684998127060) -q=10000
# see below for details

. lib

# so we always record non-canonized patches
pwd="$PWD"
trap "cp $pwd/defaults $pwd/.darcs/" EXIT
cp .darcs/defaults .
echo ALL no-canonize >> .darcs/defaults

# expected output for all reorderings
cat >log <<EOF
No conflicts to mark.
EOF

rm -rf B
darcs init B
cd B
touch ./a
darcs record -lam 'initial state'
cd ..

rm -rf B1
darcs clone B B1
cd B1
cat >a <<EOF
G
p t
EOF
darcs record -am cyhqaafycrmqgelxxurv
darcs replace t O a
echo y | darcs amend -ap cyhqaafycrmqgelxxurv

darcs replace G Z a
darcs record -am tyqredtlxtijcwmnpeyw
cat >a <<EOF
Z
u k
EOF
echo y | darcs amend -am tyqredtlxtijcwmnpeyw
cd ..

rm -rf B2
darcs clone B B2
cd B2
cat >a <<EOF
p t
t
EOF
darcs record -am lawjerjcdzhyobidfalw
cat >a <<EOF
s
t
EOF
echo y | darcs amend -ap lawjerjcdzhyobidfalw
cd ..

rm -rf R1
darcs clone B R1
cd R1
darcs pull -a --allow-conflicts ../B1 -p cyhqaafycrmqgelxxurv
darcs pull -a --allow-conflicts ../B2 -p lawjerjcdzhyobidfalw
darcs pull -a --allow-conflicts ../B1 -p tyqredtlxtijcwmnpeyw
#depend on tyqredtlxtijcwmnpeyw and lawjerjcdzhyobidfalw
echo yyd | darcs record --ask-deps -m vveiqeofcxzrtrbsyxkq
darcs mark-conflicts >log 2>&1
darcs whatsnew >>log
cd ..

rm -rf R2
darcs clone B R2
cd R2
darcs pull -a --allow-conflicts ../B2 -p lawjerjcdzhyobidfalw
darcs pull -a --allow-conflicts ../B1 -p cyhqaafycrmqgelxxurv
darcs pull -a --allow-conflicts ../B1 -p tyqredtlxtijcwmnpeyw
darcs pull -a --allow-conflicts ../R1 -p vveiqeofcxzrtrbsyxkq
darcs mark-conflicts >log 2>&1
darcs whatsnew >>log
cd ..

diff R1/log R2/log

exit # success

# Beautified complete output of the failing QC test case

Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 2338 tests and 5 shrinks):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [] [ "Z" , "p O" ])
         } :>:
         (Prim
            { unPrim =
                FP
                  (AnchoredPath [ Name { unName = "a" } ])
                  (Hunk 2 [ "p O" ] [ "u k" ])
            } :>:
            NilFL))
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [] [ "p O" , "O" ])
         } :>:
         (Prim
            { unPrim =
                FP
                  (AnchoredPath [ Name { unName = "a" } ]) (Hunk 1 [ "p O" ] [ "s" ])
            } :>:
            NilFL))
  ]
]

r2=

[]

for patches

patch 9f88ca6fc4cc5bb609273260191bb36d9743cd75
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * cyhqaafycrmqgelxxurv
hunk ./a 1
+G
+p t
replace ./a [A-Za-z_0-9] t O
patch 324a26cca1536a76b5b279d1fc6d46e78b81ca6a
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * lawjerjcdzhyobidfalw
conflictor
hash -20 ef9bd40f01457f56f489b06b621ddc913172c77b
hunk ./a 1
-G
-p O
v v v v v v v
hash 20 ef9bd40f01457f56f489b06b621ddc913172c77b
hunk ./a 1
+G
+p O
*************
hash 11 b7bb145a8f75b82ab932df5a9e1e5c4c95fb89c9
hunk ./a 1
+p O
+O
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 20 ef9bd40f01457f56f489b06b621ddc913172c77b
hunk ./a 1
+G
+p O
*************
hash 25 421f502eae08d9bd4fd7098fa4c8fc777fb3ed35
hunk ./a 1
+s
+O
^ ^ ^ ^ ^ ^ ^
patch 92281481e69782b95a5fb3283afc7260e6945635
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * tyqredtlxtijcwmnpeyw
replace ./a [A-Za-z_0-9] G Z
conflictor
v v v v v v v
hash 11 b7bb145a8f75b82ab932df5a9e1e5c4c95fb89c9
hunk ./a 1
+p O
+O
*************
hash 25 421f502eae08d9bd4fd7098fa4c8fc777fb3ed35
hunk ./a 1
+s
+O
*************
hash 11 8c36e2cc520dfd81f3343e8ef3ee04a8f6346daa
hunk ./a 1
+Z
+u k
^ ^ ^ ^ ^ ^ ^
patch 6c0596d530069fd1fb7e6217edafad2506893ee8
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * vveiqeofcxzrtrbsyxkq
depend 92281481e69782b95a5fb3283afc7260e6945635
  * tyqredtlxtijcwmnpeyw
depend 324a26cca1536a76b5b279d1fc6d46e78b81ca6a
  * lawjerjcdzhyobidfalw

versus

patch 324a26cca1536a76b5b279d1fc6d46e78b81ca6a
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * lawjerjcdzhyobidfalw
hunk ./a 1
+p t
+t
hunk ./a 1
-p t
+s
patch 9f88ca6fc4cc5bb609273260191bb36d9743cd75
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * cyhqaafycrmqgelxxurv
conflictor
hash -25 421f502eae08d9bd4fd7098fa4c8fc777fb3ed35
hunk ./a 1
-s
+p t
hash -11 b7bb145a8f75b82ab932df5a9e1e5c4c95fb89c9
hunk ./a 1
-p t
-t
v v v v v v v
hash 11 b7bb145a8f75b82ab932df5a9e1e5c4c95fb89c9
hunk ./a 1
+p t
+t
*************
hash 25 421f502eae08d9bd4fd7098fa4c8fc777fb3ed35
hunk ./a 1
+s
+t
*************
hash 20 ef9bd40f01457f56f489b06b621ddc913172c77b
hunk ./a 1
+G
+p t
^ ^ ^ ^ ^ ^ ^
replace ./a [A-Za-z_0-9] t O
patch 92281481e69782b95a5fb3283afc7260e6945635
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * tyqredtlxtijcwmnpeyw
replace ./a [A-Za-z_0-9] G Z
conflictor
v v v v v v v
hash 11 b7bb145a8f75b82ab932df5a9e1e5c4c95fb89c9
hunk ./a 1
+p O
+O
*************
hash 25 421f502eae08d9bd4fd7098fa4c8fc777fb3ed35
hunk ./a 1
+s
+O
*************
hash 11 8c36e2cc520dfd81f3343e8ef3ee04a8f6346daa
hunk ./a 1
+Z
+u k
^ ^ ^ ^ ^ ^ ^
patch 6c0596d530069fd1fb7e6217edafad2506893ee8
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * vveiqeofcxzrtrbsyxkq
depend 92281481e69782b95a5fb3283afc7260e6945635
  * tyqredtlxtijcwmnpeyw
depend 324a26cca1536a76b5b279d1fc6d46e78b81ca6a
  * lawjerjcdzhyobidfalw

Sealed2
  (WithStartState2
     (WithNames V1Model [ File "a" [] ] [])
     (SeqMS
        (ParMS
           (SeqMS
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "cyhqaafycrmqgelxxurv"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 20 ef9bd40f01457f56f489b06b621ddc913172c77b)
                       Prim
                         { unPrim =
                             FP
                               (AnchoredPath [ Name { unName = "a" } ])
                               (Hunk 1 [] [ "G" , "p t" ])
                         } :>:
                       (PrimWithName
                          (PrimPatchId 13 43be3fbfc956ba067b3aa3f52b9e57944fc80281)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "a" } ])
                                  (TokReplace "A-Za-z_0-9" "t" "O")
                            } :>:
                          NilFL))))
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "tyqredtlxtijcwmnpeyw"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 []
                 (PrimWithName
                    (PrimPatchId 22 d321382f12821eb87f7dd5f0ef4b8da4a37e164a)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (TokReplace "A-Za-z_0-9" "G" "Z")
                      } :>:
                    (PrimWithName
                       (PrimPatchId 11 8c36e2cc520dfd81f3343e8ef3ee04a8f6346daa)
                       Prim
                         { unPrim =
                             FP
                               (AnchoredPath [ Name { unName = "a" } ])
                               (Hunk 2 [ "p O" ] [ "u k" ])
                         } :>:
                       NilFL))))
           (SeqMS
              NilMS
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "lawjerjcdzhyobidfalw"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 []
                 (PrimWithName
                    (PrimPatchId 11 b7bb145a8f75b82ab932df5a9e1e5c4c95fb89c9)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 1 [] [ "p t" , "t" ])
                      } :>:
                    (PrimWithName
                       (PrimPatchId 25 421f502eae08d9bd4fd7098fa4c8fc777fb3ed35)
                       Prim
                         { unPrim =
                             FP
                               (AnchoredPath [ Name { unName = "a" } ]) (Hunk 1 [ "p t" ] [ "s" ])
                         } :>:
                       NilFL)))))
        (NamedP
           PatchInfo
             { _piDate = "20240606010532"
             , _piName = "vveiqeofcxzrtrbsyxkq"
             , _piAuthor = "tester"
             , _piLog = []
             , _piLegacyIsInverted = False
             }
           [ PatchInfo
               { _piDate = "20240606010532"
               , _piName = "tyqredtlxtijcwmnpeyw"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           , PatchInfo
               { _piDate = "20240606010532"
               , _piName = "lawjerjcdzhyobidfalw"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           ]
           NilFL)))

(used seed 5663187684998127060)
