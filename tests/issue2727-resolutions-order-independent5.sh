#!/usr/bin/env bash

. lib

if grep myers .darcs/defaults; then
  skip-formats darcs-1
fi

# so we always record non-canonized patches
pwd="$PWD"
trap "cp $pwd/defaults $pwd/.darcs/" EXIT
cp .darcs/defaults .
echo ALL no-canonize >> .darcs/defaults

rm -rf B
darcs init B
cd B
mkdir a
cat >./b <<EOF








q E
w N
e
C C


EOF
darcs record -lam 'initial state'
cd ..

rm -rf B1
darcs clone B B1
cd B1
darcs move ./a ./pPF
darcs replace G n ./b
darcs record -am icodxgoobrqkafkptdwr
cd ..

rm -rf B2
darcs clone B B2
cd B2
cat >./b <<EOF




U r

EOF
darcs record -am hlluqtinwjoxdunvzoqi
cat >./b <<EOF

J h


U r

EOF
echo y | darcs amend -a -p hlluqtinwjoxdunvzoqi
cd ..

rm -rf B3
darcs clone B B3
cd B3
cat >./b <<EOF

E o
D

q E
w N
e
C C


EOF
darcs record -am angtickssosgnvqxpbgw
cat >./b <<EOF

E o
D

q E
w N
e
C C
O
E K
V
W k
b k
I Z
p
EOF
echo y | darcs amend -a -p angtickssosgnvqxpbgw
cd ..

rm -rf B4
darcs clone B B4
cd B4
darcs move ./a ./LQ
cat >./b <<EOF
J
R v
p
U C
F
W P
Z
k M
W D
T s
h
E o
M G
Y m
j p
F
S
p Q
c y
G p







q E
w N
e
C C


EOF
darcs record -am fxhzfgiulmnzgaojznqo
cd ..

# irrelevant
# rm -rf B5
# darcs clone B B5
# cd B5
# mkdir ./a/t
# darcs record -lam dofgshqaobnokmjqygyu
# cd ..

rm -rf R1
darcs clone B R1
cd R1
darcs pull -a --allow-conflicts ../B2 -p hlluqtinwjoxdunvzoqi
darcs pull -a --allow-conflicts ../B1 -p icodxgoobrqkafkptdwr
darcs pull -a --allow-conflicts ../B4 -p fxhzfgiulmnzgaojznqo
darcs pull -a --allow-conflicts ../B3 -p angtickssosgnvqxpbgw
# irrelevant
# darcs pull -a --allow-conflicts ../B5 -p dofgshqaobnokmjqygyu

cat >>./b <<EOF
A F
p w
p
EOF
# depend on icodxgoobrqkafkptdwr
# depend on hlluqtinwjoxdunvzoqi
# depend on fxhzfgiulmnzgaojznqo
# Again, this doesn't work
# echo nwyyyd | darcs record --ask-deps -am fmujbnoeyizgucisozes
# Workaround:
# depend on hlluqtinwjoxdunvzoqi
# first 'n' is for the hunk
echo nnnnyd | darcs record --ask-deps -m intermediate
# depend on icodxgoobrqkafkptdwr
# depend on intermediate
# depend on fxhzfgiulmnzgaojznqo
echo yyyd | darcs record --ask-deps -am fmujbnoeyizgucisozes
darcs mark-conflicts >log 2>&1
darcs whatsnew >>log || not darcs whatsnew >>log
cd ..

rm -rf R2
darcs clone B R2
cd R2
darcs pull -a --allow-conflicts ../R1 -p icodxgoobrqkafkptdwr
darcs pull -a --allow-conflicts ../R1 -p fxhzfgiulmnzgaojznqo
darcs pull -a --allow-conflicts ../R1 -p hlluqtinwjoxdunvzoqi
darcs pull -a --allow-conflicts ../R1 -p angtickssosgnvqxpbgw
# irrelevant
# darcs pull -a --allow-conflicts ../R1 -p dofgshqaobnokmjqygyu
darcs pull -a --allow-conflicts ../R1 -p fmujbnoeyizgucisozes
darcs mark-conflicts >log 2>&1
darcs whatsnew >>log || not darcs whatsnew >>log
cd ..

# In R1 we see
# move ./a ./pPF, from icodxgoobrqkafkptdwr
# versus
# move ./a ./LQ, from fxhzfgiulmnzgaojznqo
# In R2 this conflict is seen as resolved, rightly so,
# since fmujbnoeyizgucisozes transitively explicitly depends
# on both.
# With tracing one can see that the inputs to RepoPatchV3.resolveConflicts are
# the same, except that the internal order in 'resolved' (10 patches in both
# R1 and R2) differs. So this is a bug in RepoPatchV3.resolveConflicts!

diff -u R1/log R2/log >&2

exit # success

# Beautified complete output of the failing QC test case

with -q=100000
Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 15498 tests and 52 shrinks):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim =
             Move
               (AnchoredPath [ Name { unName = "a" } ])
               (AnchoredPath [ Name { unName = "pPF" } ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             Move
               (AnchoredPath [ Name { unName = "a" } ])
               (AnchoredPath [ Name { unName = "LQ" } ])
         } :>:
         NilFL)
  ]
, [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (Hunk 21 [ "" , "" , "" , "" , "" , "" ] [ "E o" , "D" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ]) (Hunk 21 [ "" ] [ "J h" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (Hunk
                  24
                  [ "" , "" , "" , "" , "q E" , "w N" , "e" , "C C" , "" ]
                  [ "U r" ])
         } :>:
         NilFL)
  ]
]

r2=

[ [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (Hunk 21 [ "" , "" , "" , "" , "" , "" ] [ "E o" , "D" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ]) (Hunk 21 [ "" ] [ "J h" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ])
               (Hunk
                  24
                  [ "" , "" , "" , "" , "q E" , "w N" , "e" , "C C" , "" ]
                  [ "U r" ])
         } :>:
         NilFL)
  ]
]

for patches

patch a76aa8828b0ec2e8e3d5cc4892d143ec1dd989eb
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * hlluqtinwjoxdunvzoqi
hunk ./b 5
-
-
-
-
-q E
-w N
-e
-C C
-
+U r
hunk ./b 2
-
+J h
patch c4ffab4c9f7e77946aea095e503c8560693bb55b
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * icodxgoobrqkafkptdwr
move ./a ./pPF
replace ./b [A-Za-z_0-9] G n
patch daafdb7bcf593a09e7851509261775a4c8bbfe17
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * fxhzfgiulmnzgaojznqo
conflictor
hash -20 1c5266057ed19c48edeffb661b55d0258cf24293
move ./pPF ./a
v v v v v v v
hash 20 1c5266057ed19c48edeffb661b55d0258cf24293
move ./a ./pPF
*************
hash 65 06b0727251abb54cbb32df731daa2845e3087de4
move ./a ./LQ
^ ^ ^ ^ ^ ^ ^
hunk ./b 1
-
+J
+R v
+p
+U C
+F
+W P
+Z
+k M
+W D
+T s
+h
+E o
+M n
+Y m
+j p
+F
+S
+p Q
+c y
+n p
patch 6e7e5f9b43099f1de0ee0a26a6ae418a0b6f6a9e
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * angtickssosgnvqxpbgw
conflictor
hash -59 8be3b3901e5737a34b93d153a48978f4103008d8
hunk ./b 21
-J h
+
hash -90 84e09ac2f5e73d35f90726422b70e2e5857af0d4
hunk ./b 24
-U r
+
+
+
+
+q E
+w N
+e
+C C
+
v v v v v v v
hash 59 8be3b3901e5737a34b93d153a48978f4103008d8
hunk ./b 21
-
+J h
*************
hash 90 84e09ac2f5e73d35f90726422b70e2e5857af0d4
hunk ./b 24
-
-
-
-
-q E
-w N
-e
-C C
-
+U r
*************
hash 49 e82d51ebb8529f70a0bbf002eecbd32f3a45e1e9
hunk ./b 21
-
-
-
-
-
-
+E o
+D
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 90 84e09ac2f5e73d35f90726422b70e2e5857af0d4
hunk ./b 24
-
-
-
-
-q E
-w N
-e
-C C
-
+U r
*************
hash 54 6b11fc5e2a329e572e08eac6f9d04ede137232e2
hunk ./b 32
-
-
+O
+E K
+V
+W k
+b k
+I Z
+p
^ ^ ^ ^ ^ ^ ^
patch b393b57809990125ddefcd1950bb3ae337e38b4b
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * dofgshqaobnokmjqygyu
adddir ./a/t
patch 4bfaee19885b6d7c79bd01bdf61d35bd463a0c79
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * fmujbnoeyizgucisozes
depend c4ffab4c9f7e77946aea095e503c8560693bb55b
  * icodxgoobrqkafkptdwr
depend a76aa8828b0ec2e8e3d5cc4892d143ec1dd989eb
  * hlluqtinwjoxdunvzoqi
depend daafdb7bcf593a09e7851509261775a4c8bbfe17
  * fxhzfgiulmnzgaojznqo
hunk ./b 34
+A F
+p w
+p

versus

patch c4ffab4c9f7e77946aea095e503c8560693bb55b
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * icodxgoobrqkafkptdwr
move ./a ./pPF
replace ./b [A-Za-z_0-9] G n
patch daafdb7bcf593a09e7851509261775a4c8bbfe17
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * fxhzfgiulmnzgaojznqo
conflictor
hash -20 1c5266057ed19c48edeffb661b55d0258cf24293
move ./pPF ./a
v v v v v v v
hash 20 1c5266057ed19c48edeffb661b55d0258cf24293
move ./a ./pPF
*************
hash 65 06b0727251abb54cbb32df731daa2845e3087de4
move ./a ./LQ
^ ^ ^ ^ ^ ^ ^
hunk ./b 1
-
+J
+R v
+p
+U C
+F
+W P
+Z
+k M
+W D
+T s
+h
+E o
+M n
+Y m
+j p
+F
+S
+p Q
+c y
+n p
patch a76aa8828b0ec2e8e3d5cc4892d143ec1dd989eb
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * hlluqtinwjoxdunvzoqi
hunk ./b 24
-
-
-
-
-q E
-w N
-e
-C C
-
+U r
hunk ./b 21
-
+J h
patch 6e7e5f9b43099f1de0ee0a26a6ae418a0b6f6a9e
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * angtickssosgnvqxpbgw
conflictor
hash -59 8be3b3901e5737a34b93d153a48978f4103008d8
hunk ./b 21
-J h
+
hash -90 84e09ac2f5e73d35f90726422b70e2e5857af0d4
hunk ./b 24
-U r
+
+
+
+
+q E
+w N
+e
+C C
+
v v v v v v v
hash 59 8be3b3901e5737a34b93d153a48978f4103008d8
hunk ./b 21
-
+J h
*************
hash 90 84e09ac2f5e73d35f90726422b70e2e5857af0d4
hunk ./b 24
-
-
-
-
-q E
-w N
-e
-C C
-
+U r
*************
hash 49 e82d51ebb8529f70a0bbf002eecbd32f3a45e1e9
hunk ./b 21
-
-
-
-
-
-
+E o
+D
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 90 84e09ac2f5e73d35f90726422b70e2e5857af0d4
hunk ./b 24
-
-
-
-
-q E
-w N
-e
-C C
-
+U r
*************
hash 54 6b11fc5e2a329e572e08eac6f9d04ede137232e2
hunk ./b 32
-
-
+O
+E K
+V
+W k
+b k
+I Z
+p
^ ^ ^ ^ ^ ^ ^
patch b393b57809990125ddefcd1950bb3ae337e38b4b
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * dofgshqaobnokmjqygyu
adddir ./a/t
patch 4bfaee19885b6d7c79bd01bdf61d35bd463a0c79
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * fmujbnoeyizgucisozes
depend c4ffab4c9f7e77946aea095e503c8560693bb55b
  * icodxgoobrqkafkptdwr
depend a76aa8828b0ec2e8e3d5cc4892d143ec1dd989eb
  * hlluqtinwjoxdunvzoqi
depend daafdb7bcf593a09e7851509261775a4c8bbfe17
  * fxhzfgiulmnzgaojznqo
hunk ./b 34
+A F
+p w
+p

Sealed2
  (WithStartState2
     (WithNames
        V1Model
        [ Dir "a"
        , File
            "b"
            [ ""
            , ""
            , ""
            , ""
            , ""
            , ""
            , ""
            , ""
            , "q E"
            , "w N"
            , "e"
            , "C C"
            , ""
            , ""
            ]
        ]
        [])
     (SeqMS
        (ParMS
           (ParMS
              (ParMS
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "icodxgoobrqkafkptdwr"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 20 1c5266057ed19c48edeffb661b55d0258cf24293)
                          Prim
                            { unPrim =
                                Move
                                  (AnchoredPath [ Name { unName = "a" } ])
                                  (AnchoredPath [ Name { unName = "pPF" } ])
                            } :>:
                          (PrimWithName
                             (PrimPatchId 11 2eabc4deab38658effa885b7e3f495b4877191ee)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "b" } ])
                                     (TokReplace "A-Za-z_0-9" "G" "n")
                               } :>:
                             NilFL))))
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "hlluqtinwjoxdunvzoqi"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 90 84e09ac2f5e73d35f90726422b70e2e5857af0d4)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "b" } ])
                                  (Hunk
                                     5
                                     [ "" , "" , "" , "" , "q E" , "w N" , "e" , "C C" , "" ]
                                     [ "U r" ])
                            } :>:
                          (PrimWithName
                             (PrimPatchId 59 8be3b3901e5737a34b93d153a48978f4103008d8)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "b" } ])
                                     (Hunk 2 [ "" ] [ "J h" ])
                               } :>:
                             NilFL)))))
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "fxhzfgiulmnzgaojznqo"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 65 06b0727251abb54cbb32df731daa2845e3087de4)
                       Prim
                         { unPrim =
                             Move
                               (AnchoredPath [ Name { unName = "a" } ])
                               (AnchoredPath [ Name { unName = "LQ" } ])
                         } :>:
                       (PrimWithName
                          (PrimPatchId 16 b1cf48180a70c89defa9f2010c3b57e9c80c9071)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "b" } ])
                                  (Hunk
                                     1
                                     [ "" ]
                                     [ "J"
                                     , "R v"
                                     , "p"
                                     , "U C"
                                     , "F"
                                     , "W P"
                                     , "Z"
                                     , "k M"
                                     , "W D"
                                     , "T s"
                                     , "h"
                                     , "E o"
                                     , "M G"
                                     , "Y m"
                                     , "j p"
                                     , "F"
                                     , "S"
                                     , "p Q"
                                     , "c y"
                                     , "G p"
                                     ])
                            } :>:
                          NilFL)))))
           (ParMS
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "angtickssosgnvqxpbgw"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 49 e82d51ebb8529f70a0bbf002eecbd32f3a45e1e9)
                       Prim
                         { unPrim =
                             FP
                               (AnchoredPath [ Name { unName = "b" } ])
                               (Hunk 2 [ "" , "" , "" , "" , "" , "" ] [ "E o" , "D" ])
                         } :>:
                       (PrimWithName
                          (PrimPatchId 54 6b11fc5e2a329e572e08eac6f9d04ede137232e2)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "b" } ])
                                  (Hunk
                                     9
                                     [ "" , "" ]
                                     [ "O" , "E K" , "V" , "W k" , "b k" , "I Z" , "p" ])
                            } :>:
                          NilFL))))
              (SeqMS
                 NilMS
                 (NamedP
                    PatchInfo
                      { _piDate = "20240606010532"
                      , _piName = "dofgshqaobnokmjqygyu"
                      , _piAuthor = "tester"
                      , _piLog = []
                      , _piLegacyIsInverted = False
                      }
                    []
                    (PrimWithName
                       (PrimPatchId 16 460ef68ae545f13c99bfba068017c6035fc8ec3d)
                       Prim
                         { unPrim =
                             DP
                               (AnchoredPath [ Name { unName = "a" } , Name { unName = "t" } ])
                               AddDir
                         } :>:
                       NilFL)))))
        (NamedP
           PatchInfo
             { _piDate = "20240606010532"
             , _piName = "fmujbnoeyizgucisozes"
             , _piAuthor = "tester"
             , _piLog = []
             , _piLegacyIsInverted = False
             }
           [ PatchInfo
               { _piDate = "20240606010532"
               , _piName = "icodxgoobrqkafkptdwr"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           , PatchInfo
               { _piDate = "20240606010532"
               , _piName = "hlluqtinwjoxdunvzoqi"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           , PatchInfo
               { _piDate = "20240606010532"
               , _piName = "fxhzfgiulmnzgaojznqo"
               , _piAuthor = "tester"
               , _piLog = []
               , _piLegacyIsInverted = False
               }
           ]
           (PrimWithName
              (PrimPatchId 79 cabd8f4de1367c0461b06979a3309d2e42f1838f)
              Prim
                { unPrim =
                    FP
                      (AnchoredPath [ Name { unName = "b" } ])
                      (Hunk 34 [] [ "A F" , "p w" , "p" ])
                } :>:
              NilFL))))

(used seed 6479663611722759425)
