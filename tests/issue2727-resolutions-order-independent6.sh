#!/usr/bin/env bash

. lib

rm -rf B
darcs init B
cd B
mkdir ./a
touch ./b
darcs record -lam 'initial state'
cd ..

# 4 branches with 1 patch each

rm -rf B1
darcs clone B B1
cd B1
rmdir ./a
darcs record -am b1
cd ..

rm -rf B2
darcs clone B B2
cd B2
cat >./b <<EOF
w
EOF
darcs record -am b2
cd ..

rm -rf B3
darcs clone B B3
cd B3
cat >./b <<EOF
P
Y
EOF
darcs record -am b3
cd ..

# duplicate of b1
rm -rf B4
darcs clone B B4
cd B4
rmdir ./a
darcs record -am b4
cd ..

rm -rf R1
darcs clone B R1
cd R1
# context
darcs pull --allow-conflicts ../B3 -a
darcs pull --allow-conflicts ../B1 -a
darcs pull --allow-conflicts ../B2 -a
# patches
darcs pull --mark-conflicts ../B4 -a >log 2>&1
not darcs whatsnew >>log
cd ..

rm -rf R2
darcs clone B R2
cd R2
# context
darcs pull --allow-conflicts ../B2 -a
darcs pull --allow-conflicts ../B3 -a
darcs pull --allow-conflicts ../B1 -a
darcs pull --mark-conflicts ../B4 -a >log 2>&1
not darcs whatsnew >>log
cd ..

diff -u R1/log R2/log >&2

exit; # success

RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 168 tests and 34 shrinks):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
         NilFL)
  ]
, [ Sealed
      (Prim
         { unPrim =
             FP (AnchoredPath [ Name { unName = "b" } ]) (Hunk 1 [] [ "w" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "b" } ]) (Hunk 1 [] [ "P" , "Y" ])
         } :>:
         NilFL)
  ]
]

r2=

[ [ Sealed
      (Prim
         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir } :>:
         NilFL)
  ]
]

for context

hash 67 34a0f2ded1b229162692b24a3fd66fa8b9731467
hunk ./b 1
+P
+Y
hash 45 5c42bf7895717660a24749fcbf5b02ddf81b95aa
rmdir ./a
conflictor
hash -67 34a0f2ded1b229162692b24a3fd66fa8b9731467
hunk ./b 1
-P
-Y
v v v v v v v
hash 67 34a0f2ded1b229162692b24a3fd66fa8b9731467
hunk ./b 1
+P
+Y
*************
hash 62 5fe16ca909b66e466b57a292d038c4fda79b3d2d
hunk ./b 1
+w
^ ^ ^ ^ ^ ^ ^

and patches

conflictor
hash -45 5c42bf7895717660a24749fcbf5b02ddf81b95aa
adddir ./a
v v v v v v v
hash 45 5c42bf7895717660a24749fcbf5b02ddf81b95aa
rmdir ./a
*************
hash 51 e96ccc18e8b980e2f1324a0421b081b065e160da
rmdir ./a
^ ^ ^ ^ ^ ^ ^

versus

for context

hash 62 5fe16ca909b66e466b57a292d038c4fda79b3d2d
hunk ./b 1
+w
conflictor
hash -62 5fe16ca909b66e466b57a292d038c4fda79b3d2d
hunk ./b 1
-w
v v v v v v v
hash 62 5fe16ca909b66e466b57a292d038c4fda79b3d2d
hunk ./b 1
+w
*************
hash 67 34a0f2ded1b229162692b24a3fd66fa8b9731467
hunk ./b 1
+P
+Y
^ ^ ^ ^ ^ ^ ^
hash 45 5c42bf7895717660a24749fcbf5b02ddf81b95aa
rmdir ./a

and patches

conflictor
hash -45 5c42bf7895717660a24749fcbf5b02ddf81b95aa
adddir ./a
v v v v v v v
hash 45 5c42bf7895717660a24749fcbf5b02ddf81b95aa
rmdir ./a
*************
hash 51 e96ccc18e8b980e2f1324a0421b081b065e160da
rmdir ./a
^ ^ ^ ^ ^ ^ ^

Sealed2
  (WithStartState2
     V1Model
     [ Dir "a" , File "b" [] ]
     (WithSplit
        1
        (ParMS
           (ParMS
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 45 5c42bf7895717660a24749fcbf5b02ddf81b95aa)
                    Prim
                      { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir }))
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 62 5fe16ca909b66e466b57a292d038c4fda79b3d2d)
                    Prim
                      { unPrim =
                          FP (AnchoredPath [ Name { unName = "b" } ]) (Hunk 1 [] [ "w" ])
                      })))
           (ParMS
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 67 34a0f2ded1b229162692b24a3fd66fa8b9731467)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "b" } ]) (Hunk 1 [] [ "P" , "Y" ])
                      }))
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 51 e96ccc18e8b980e2f1324a0421b081b065e160da)
                    Prim
                      { unPrim = DP (AnchoredPath [ Name { unName = "a" } ]) RmDir
                      }))))))

(use -r=5572661293594798996 and -q=1000)
