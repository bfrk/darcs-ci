#!/usr/bin/env bash

. lib

# Test fails for darcs-1 patches. We can't be bothered to fix those.
skip-formats darcs-1

rm -rf B
darcs init B
cd B
cat >./a <<EOF



EOF
darcs record -lam 'initial state'
cd ..

# 4 branches with 1 patch each

rm -rf B1
darcs clone B B1
cd B1
cat >./a <<EOF
L
w

EOF
darcs record -am b1
cd ..

rm -rf B2
darcs clone B B2
cd B2
cat >./a <<EOF

L x
EOF
darcs record -am b2
cd ..

rm -rf B3
darcs clone B B3
cd B3
cat >./a <<EOF


t
w
EOF
darcs record -am b3
cd ..

rm -rf B4
darcs clone B B4
cd B4
cat >./a <<EOF
k C
U W
L
f P
A
A


EOF
darcs record -am b4
cd ..

rm -rf R1
darcs clone B R1
cd R1
# context
darcs pull --allow-conflicts ../B1 -a
darcs pull --allow-conflicts ../B2 -a
darcs pull --allow-conflicts ../B3 -a
# patches
darcs pull --mark-conflicts ../B4 -a 2>&1 | grep -v 'Backing up' > log
darcs whatsnew >>log
cd ..

rm -rf R2
darcs clone B R2
cd R2
# context
darcs pull --allow-conflicts ../B2 -a
darcs pull --allow-conflicts ../B1 -a
darcs pull --allow-conflicts ../B3 -a
darcs pull --mark-conflicts ../B4 -a 2>&1 | grep -v 'Backing up' > log
darcs whatsnew >>log
cd ..

diff -u R1/log R2/log >&2

exit; # success

RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 4457 tests and 25 shrinks):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [ "" ] [ "k C" , "U W" , "L" , "f P" , "A" , "A" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [ "" , "" ] [ "L" , "w" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 2 [ "" , "" ] [ "L x" ])
         } :>:
         NilFL)
  ]
]

r2=

[ [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [ "" ] [ "k C" , "U W" , "L" , "f P" , "A" , "A" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 3 [ "" ] [ "t" , "w" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [ "" , "" ] [ "L" , "w" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 2 [ "" , "" ] [ "L x" ])
         } :>:
         NilFL)
  ]
]

for context

hash 30 fca312e3b62378403077619bfdab030feb8a1190
hunk ./a 1
-
-
+L
+w
conflictor
hash -30 fca312e3b62378403077619bfdab030feb8a1190
hunk ./a 1
-L
-w
+
+
v v v v v v v
hash 30 fca312e3b62378403077619bfdab030feb8a1190
hunk ./a 1
-
-
+L
+w
*************
hash 35 33afb5708f81f4dfa5bbc58664ab3a58ebdb57ff
hunk ./a 2
-
-
+L x
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 35 33afb5708f81f4dfa5bbc58664ab3a58ebdb57ff
hunk ./a 2
-
-
+L x
*************
hash 9 a1d3de47f5195b3b6d2fd141760f78a27e97b105
hunk ./a 3
-
+t
+w
^ ^ ^ ^ ^ ^ ^

and patches

conflictor
v v v v v v v
hash 30 fca312e3b62378403077619bfdab030feb8a1190
hunk ./a 1
-
-
+L
+w
*************
hash 2 25b39f6a7888bce66be38bf70d9af2dfa169c986
hunk ./a 1
-
+k C
+U W
+L
+f P
+A
+A
^ ^ ^ ^ ^ ^ ^

versus

for context

hash 35 33afb5708f81f4dfa5bbc58664ab3a58ebdb57ff
hunk ./a 2
-
-
+L x
conflictor
hash -35 33afb5708f81f4dfa5bbc58664ab3a58ebdb57ff
hunk ./a 2
-L x
+
+
v v v v v v v
hash 35 33afb5708f81f4dfa5bbc58664ab3a58ebdb57ff
hunk ./a 2
-
-
+L x
*************
hash 30 fca312e3b62378403077619bfdab030feb8a1190
hunk ./a 1
-
-
+L
+w
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 35 33afb5708f81f4dfa5bbc58664ab3a58ebdb57ff
hunk ./a 2
-
-
+L x
*************
hash 9 a1d3de47f5195b3b6d2fd141760f78a27e97b105
hunk ./a 3
-
+t
+w
^ ^ ^ ^ ^ ^ ^

and patches

conflictor
v v v v v v v
hash 30 fca312e3b62378403077619bfdab030feb8a1190
hunk ./a 1
-
-
+L
+w
*************
hash 2 25b39f6a7888bce66be38bf70d9af2dfa169c986
hunk ./a 1
-
+k C
+U W
+L
+f P
+A
+A
^ ^ ^ ^ ^ ^ ^

Sealed2
  (WithStartState2
     V1Model
     [ File "a" [ "" , "" , "" ] ]
     (WithSplit
        1
        (ParMS
           (ParMS
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 30 fca312e3b62378403077619bfdab030feb8a1190)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 1 [ "" , "" ] [ "L" , "w" ])
                      }))
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 35 33afb5708f81f4dfa5bbc58664ab3a58ebdb57ff)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 2 [ "" , "" ] [ "L x" ])
                      })))
           (ParMS
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 9 a1d3de47f5195b3b6d2fd141760f78a27e97b105)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 3 [ "" ] [ "t" , "w" ])
                      }))
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 2 25b39f6a7888bce66be38bf70d9af2dfa169c986)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 1 [ "" ] [ "k C" , "U W" , "L" , "f P" , "A" , "A" ])
                      }))))))

(used seed -7633521189062312427) -q=10000
