#!/usr/bin/env bash

# This one is interesting. The difference in resolutions here is purely one
# between two separate conflicts versus one large, otherwise they are
# identical.

. lib

skip-formats darcs-1

rm -rf B
darcs init B
cd B
cat >./a <<EOF




EOF
darcs record -lam 'initial state'
cd ..

# 4 branches

rm -rf B1
darcs clone B B1
cd B1
cat >./a <<EOF


r
L
g H
e
EOF
darcs record -am b1
cd ..

rm -rf B2
darcs clone B B2
cd B2
cat >./a <<EOF

N P
a E

EOF
darcs record -am b2
cd ..

rm -rf B3
darcs clone B B3
cd B3
cat >./a <<EOF
W
X U
t b
r
b q
w


EOF
darcs record -am b31
cat >./a <<EOF
W
X U
t b
r
b q
w

a E
A v
Y
d n
N S
H
w o
F Z
R g
g H
e
EOF
darcs record -am b32
cd ..

rm -rf B4
darcs clone B B4
cd B4
cat >./a <<EOF
K b
u
p
I i
d U
W
R d
e
g f



EOF
darcs record -am b4
cd ..

rm -rf R1
darcs clone B R1
cd R1
darcs pull --allow-conflicts -a ../B2 -p b2
darcs pull --allow-conflicts -a ../B1 -p b1
darcs pull --allow-conflicts -a ../B3 -p b31
darcs pull --mark-conflicts -a ../B4 ../B3 2>&1 | grep -v /B | grep -v 'Backing up' > log
darcs whatsnew >> log
cd ..

rm -rf R2
darcs clone B R2
cd R2
darcs pull --allow-conflicts -a ../B1 -p b1
darcs pull --allow-conflicts -a ../B3 -p b31
darcs pull --allow-conflicts -a ../B2 -p b2
darcs pull --mark-conflicts -a ../B3 ../B4 2>&1 | grep -v /B | grep -v 'Backing up' > log
darcs whatsnew >> log
cd ..

diff -u R1/log R2/log >&2

exit

RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 9489 tests and 43 shrinks):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [ "" , "" ] [ "W" , "X U" , "t b" , "r" , "b q" , "w" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk
                  1
                  [ "" ]
                  [ "K b" , "u" , "p" , "I i" , "d U" , "W" , "R d" , "e" , "g f" ])
         } :>:
         NilFL)
  ]
, [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 3 [ "" , "" ] [ "r" , "L" , "g H" , "e" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk
                  4
                  [ "" ]
                  [ "a E"
                  , "A v"
                  , "Y"
                  , "d n"
                  , "N S"
                  , "H"
                  , "w o"
                  , "F Z"
                  , "R g"
                  , "g H"
                  , "e"
                  ])
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
               (Hunk 1 [ "" , "" ] [ "W" , "X U" , "t b" , "r" , "b q" , "w" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk
                  1
                  [ "" ]
                  [ "K b" , "u" , "p" , "I i" , "d U" , "W" , "R d" , "e" , "g f" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 3 [ "" , "" ] [ "r" , "L" , "g H" , "e" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk
                  4
                  [ "" ]
                  [ "a E"
                  , "A v"
                  , "Y"
                  , "d n"
                  , "N S"
                  , "H"
                  , "w o"
                  , "F Z"
                  , "R g"
                  , "g H"
                  , "e"
                  ])
         } :>:
         NilFL)
  ]
]

for context

hash 58 c9263b1c3ee4d4c8e32683c4bb068572dcaffe96
hunk ./a 2
-
-
+N P
+a E
conflictor
hash -58 c9263b1c3ee4d4c8e32683c4bb068572dcaffe96
hunk ./a 2
-N P
-a E
+
+
v v v v v v v
hash 58 c9263b1c3ee4d4c8e32683c4bb068572dcaffe96
hunk ./a 2
-
-
+N P
+a E
*************
hash 47 f264fa4869a162df5f44340c2189a09f12e29311
hunk ./a 3
-
-
+r
+L
+g H
+e
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 58 c9263b1c3ee4d4c8e32683c4bb068572dcaffe96
hunk ./a 2
-
-
+N P
+a E
*************
hash 15 12bdd7e73e75bf2040c97f1697b4b6b7faf2a37e
hunk ./a 1
-
-
+W
+X U
+t b
+r
+b q
+w
^ ^ ^ ^ ^ ^ ^

and patches

conflictor
v v v v v v v
hash 15 12bdd7e73e75bf2040c97f1697b4b6b7faf2a37e
hunk ./a 1
-
-
+W
+X U
+t b
+r
+b q
+w
*************
hash 24 51703d6dd2cf12d87407bf7b721dda10aec9afa1
hunk ./a 1
-
+K b
+u
+p
+I i
+d U
+W
+R d
+e
+g f
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 47 f264fa4869a162df5f44340c2189a09f12e29311
hunk ./a 3
-
-
+r
+L
+g H
+e
*************
hash 81 ef0c87ff7603d48eb2cc700b114937b313fce309
hunk ./a 4
-
+a E
+A v
+Y
+d n
+N S
+H
+w o
+F Z
+R g
+g H
+e
^ ^ ^ ^ ^ ^ ^

versus

for context

hash 47 f264fa4869a162df5f44340c2189a09f12e29311
hunk ./a 3
-
-
+r
+L
+g H
+e
hash 15 12bdd7e73e75bf2040c97f1697b4b6b7faf2a37e
hunk ./a 1
-
-
+W
+X U
+t b
+r
+b q
+w
conflictor
hash -15 12bdd7e73e75bf2040c97f1697b4b6b7faf2a37e
hunk ./a 1
-W
-X U
-t b
-r
-b q
-w
+
+
hash -47 f264fa4869a162df5f44340c2189a09f12e29311
hunk ./a 3
-r
-L
-g H
-e
+
+
v v v v v v v
hash 15 12bdd7e73e75bf2040c97f1697b4b6b7faf2a37e
hunk ./a 1
-
-
+W
+X U
+t b
+r
+b q
+w
*************
hash 47 f264fa4869a162df5f44340c2189a09f12e29311
hunk ./a 3
-
-
+r
+L
+g H
+e
*************
hash 58 c9263b1c3ee4d4c8e32683c4bb068572dcaffe96
hunk ./a 2
-
-
+N P
+a E
^ ^ ^ ^ ^ ^ ^

and patches

conflictor
v v v v v v v
hash 47 f264fa4869a162df5f44340c2189a09f12e29311
hunk ./a 3
-
-
+r
+L
+g H
+e
*************
hash 81 ef0c87ff7603d48eb2cc700b114937b313fce309
hunk ./a 4
-
+a E
+A v
+Y
+d n
+N S
+H
+w o
+F Z
+R g
+g H
+e
^ ^ ^ ^ ^ ^ ^
conflictor
v v v v v v v
hash 15 12bdd7e73e75bf2040c97f1697b4b6b7faf2a37e
hunk ./a 1
-
-
+W
+X U
+t b
+r
+b q
+w
*************
hash 24 51703d6dd2cf12d87407bf7b721dda10aec9afa1
hunk ./a 1
-
+K b
+u
+p
+I i
+d U
+W
+R d
+e
+g f
^ ^ ^ ^ ^ ^ ^

Sealed2
  (WithStartState2
     V1Model
     [ File "a" [ "" , "" , "" , "" ] ]
     (WithSplit
        2
        (ParMS
           (ParMS
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 47 f264fa4869a162df5f44340c2189a09f12e29311)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 3 [ "" , "" ] [ "r" , "L" , "g H" , "e" ])
                      }))
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 58 c9263b1c3ee4d4c8e32683c4bb068572dcaffe96)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 2 [ "" , "" ] [ "N P" , "a E" ])
                      })))
           (ParMS
              (SeqMS
                 (SeqMS
                    NilMS
                    (PrimWithName
                       (PrimPatchId 15 12bdd7e73e75bf2040c97f1697b4b6b7faf2a37e)
                       Prim
                         { unPrim =
                             FP
                               (AnchoredPath [ Name { unName = "a" } ])
                               (Hunk 1 [ "" , "" ] [ "W" , "X U" , "t b" , "r" , "b q" , "w" ])
                         }))
                 (PrimWithName
                    (PrimPatchId 81 ef0c87ff7603d48eb2cc700b114937b313fce309)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk
                               8
                               [ "" ]
                               [ "a E"
                               , "A v"
                               , "Y"
                               , "d n"
                               , "N S"
                               , "H"
                               , "w o"
                               , "F Z"
                               , "R g"
                               , "g H"
                               , "e"
                               ])
                      }))
              (SeqMS
                 NilMS
                 (PrimWithName
                    (PrimPatchId 24 51703d6dd2cf12d87407bf7b721dda10aec9afa1)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk
                               1
                               [ "" ]
                               [ "K b" , "u" , "p" , "I i" , "d U" , "W" , "R d" , "e" , "g f" ])
                      }))))))

(used seed -6466895320292341779)
