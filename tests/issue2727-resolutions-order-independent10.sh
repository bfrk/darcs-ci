#!/usr/bin/env bash

. lib

# With darcs-1 this test fails due to commutation bugs; it would crash had
# we not made the algorithm more tolerant wrt such bugs.
# With darcs-2 the final diff fails because (1) conflicting alternatives
# aren't sorted and (2) it reports one alternative with an inverse pair of
# hunk 'ztsvuthkcyyrmwfyytjn 1' mixed in. The latter would be eliminated when
# we actually created markup which we can't because of the replace that's
# part of the conflict.
skip-formats darcs-1 darcs-2

rm -rf B
darcs init B
cd B
cat >a <<EOF


EOF
darcs record -lam 'initial state'
cd ..

# branches

rm -rf B1
darcs clone B B1
cd B1
cat >a <<EOF
h
p M
i d

EOF
darcs record -am 'ztsvuthkcyyrmwfyytjn 1'
cat >a <<EOF
h
p M
i d
A T
EOF
darcs record -am 'ztsvuthkcyyrmwfyytjn 2'
cd ..

rm -rf B2
darcs clone B B2
cd B2
darcs replace y T a
darcs record -am 'ntqrzqeyfiehhlogtsuc 1'
cat >a <<EOF
Z h
W i
EOF
darcs record -am 'ntqrzqeyfiehhlogtsuc 2'
cd ..

rm -rf C
darcs clone B C
cd C
darcs pull -a --allow-conflicts ../B1
darcs pull -a --allow-conflicts ../B2
cat >a <<EOF
B
I
c m
u


EOF
# simplification: this bug is actually in RepoPatchV3, not in Named
# echo yyd | darcs record --ask-deps -am otkvniorrncosojszsxh
darcs record -am otkvniorrncosojszsxh
# all conflicts should be resolved at this point
darcs mark-conflicts >log 2>&1
grep -i 'no conflicts' log
grep -vi 'cannot mark' log
cd ..

rm -rf D
darcs clone B D
cd D
darcs replace f M a
darcs record -am vqqtzxvvzjgddgwlezwj
cd ..

rm -rf R1
darcs clone B R1
cd R1
darcs pull -a --allow-conflicts ../B1 -p ztsvuthkcyyrmwfyytjn
darcs pull -a --allow-conflicts ../B2 -p ntqrzqeyfiehhlogtsuc
darcs pull -a --allow-conflicts ../C -p otkvniorrncosojszsxh
darcs pull -a --allow-conflicts ../D -p vqqtzxvvzjgddgwlezwj
# with darcs-1 format we don't even get here because the previous
# line crashes darcs
darcs mark-conflicts >log 2>&1
not darcs whatsnew >> log
cd ..

rm -rf R2
darcs clone B R2
cd R2
darcs pull -a --allow-conflicts ../B2 -p ntqrzqeyfiehhlogtsuc
darcs pull -a --allow-conflicts ../B1 -p ztsvuthkcyyrmwfyytjn
darcs pull -a --allow-conflicts ../C -p otkvniorrncosojszsxh
darcs pull -a --allow-conflicts ../D -p vqqtzxvvzjgddgwlezwj
darcs mark-conflicts >log 2>&1
not darcs whatsnew >> log
cd ..

diff -u R1/log R2/log >&2

exit

Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 53391 tests and 30 shrinks, -q=100000):
resolutions differ: r1=

[ [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [ "" ] [ "h" , "p M" , "i d" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ]) (Hunk 2 [ "" ] [ "A T" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (TokReplace "A-Za-z_0-9" "y" "T")
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [] [ "B" , "I" , "c m" , "u" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (TokReplace "A-Za-z_0-9" "f" "M")
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
               (Hunk 1 [ "" ] [ "h" , "p M" , "i d" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [] [ "B" , "I" , "c m" , "u" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (TokReplace "A-Za-z_0-9" "f" "M")
         } :>:
         NilFL)
  ]
, [ Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ]) (Hunk 2 [ "" ] [ "A T" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (TokReplace "A-Za-z_0-9" "y" "T")
         } :>:
         NilFL)
  ]
]

for context

and patches

patch f7691d00b9c98171d604308f8702123ff03e8b3b
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * ztsvuthkcyyrmwfyytjn
hash 15 845f46746f8daf575a42e4bde5cb9d65274e1d98
hunk ./a 1
-
+h
+p M
+i d
hash 19 4627349ce0608c332decd5fd80541a3f026cc576
hunk ./a 4
-
+A T
patch 17db37c2d9280b950359870756b09020861ae4c8
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * ntqrzqeyfiehhlogtsuc
conflictor
hash -19 4627349ce0608c332decd5fd80541a3f026cc576
hunk ./a 4
-A T
+
v v v v v v v
hash 19 4627349ce0608c332decd5fd80541a3f026cc576
hunk ./a 4
-
+A T
*************
hash 31 80a23b42177d6d019627de006f92129d6f950654
replace ./a [A-Za-z_0-9] y T
^ ^ ^ ^ ^ ^ ^
conflictor
hash -15 845f46746f8daf575a42e4bde5cb9d65274e1d98
hunk ./a 1
-h
-p M
-i d
+
v v v v v v v
hash 15 845f46746f8daf575a42e4bde5cb9d65274e1d98
hunk ./a 1
-
+h
+p M
+i d
*************
hash 19 4627349ce0608c332decd5fd80541a3f026cc576
hunk ./a 2
-
+A T
*************
hash 66 e9d7ccc116faef824262571d15e15b09dcb68ff9
hunk ./a 1
-
-
+Z h
+W i
^ ^ ^ ^ ^ ^ ^
patch 1123170e60fa9dae9cceb196169022ebaf07fb21
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * otkvniorrncosojszsxh
depend f7691d00b9c98171d604308f8702123ff03e8b3b
  * ztsvuthkcyyrmwfyytjn
depend 17db37c2d9280b950359870756b09020861ae4c8
  * ntqrzqeyfiehhlogtsuc
hash 33 ca5c07a3d8e20ca553cb6b83d5c2a2df0bd5e34e
hunk ./a 1
+B
+I
+c m
+u
patch 2a27683b9b6cc747116eb46c83e98d3a8be44102
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * vqqtzxvvzjgddgwlezwj
conflictor
hash -33 ca5c07a3d8e20ca553cb6b83d5c2a2df0bd5e34e
hunk ./a 1
-B
-I
-c m
-u
v v v v v v v
hash 15 845f46746f8daf575a42e4bde5cb9d65274e1d98
hunk ./a 1
-
+h
+p M
+i d
*************
hash 33 ca5c07a3d8e20ca553cb6b83d5c2a2df0bd5e34e
hunk ./a 1
+B
+I
+c m
+u
*************
hash 39 7e735b17621d8772e585be32344064c0250455c1
replace ./a [A-Za-z_0-9] f M
^ ^ ^ ^ ^ ^ ^

versus

for context

and patches

patch 17db37c2d9280b950359870756b09020861ae4c8
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * ntqrzqeyfiehhlogtsuc
hash 31 80a23b42177d6d019627de006f92129d6f950654
replace ./a [A-Za-z_0-9] y T
hash 66 e9d7ccc116faef824262571d15e15b09dcb68ff9
hunk ./a 1
-
-
+Z h
+W i
patch f7691d00b9c98171d604308f8702123ff03e8b3b
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * ztsvuthkcyyrmwfyytjn
conflictor
hash -66 e9d7ccc116faef824262571d15e15b09dcb68ff9
hunk ./a 1
-Z h
-W i
+
+
v v v v v v v
hash 66 e9d7ccc116faef824262571d15e15b09dcb68ff9
hunk ./a 1
-
-
+Z h
+W i
*************
hash 15 845f46746f8daf575a42e4bde5cb9d65274e1d98
hunk ./a 1
-
+h
+p M
+i d
^ ^ ^ ^ ^ ^ ^
conflictor
hash -31 80a23b42177d6d019627de006f92129d6f950654
replace ./a [A-Za-z_0-9] T y
v v v v v v v
hash 31 80a23b42177d6d019627de006f92129d6f950654
replace ./a [A-Za-z_0-9] y T
*************
hash 66 e9d7ccc116faef824262571d15e15b09dcb68ff9
hunk ./a 1
-
-
+Z h
+W i
*************
hash 19 4627349ce0608c332decd5fd80541a3f026cc576
hunk ./a 2
-
+A T
^ ^ ^ ^ ^ ^ ^
patch 1123170e60fa9dae9cceb196169022ebaf07fb21
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * otkvniorrncosojszsxh
depend f7691d00b9c98171d604308f8702123ff03e8b3b
  * ztsvuthkcyyrmwfyytjn
depend 17db37c2d9280b950359870756b09020861ae4c8
  * ntqrzqeyfiehhlogtsuc
hash 33 ca5c07a3d8e20ca553cb6b83d5c2a2df0bd5e34e
hunk ./a 1
+B
+I
+c m
+u
patch 2a27683b9b6cc747116eb46c83e98d3a8be44102
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * vqqtzxvvzjgddgwlezwj
conflictor
hash -33 ca5c07a3d8e20ca553cb6b83d5c2a2df0bd5e34e
hunk ./a 1
-B
-I
-c m
-u
v v v v v v v
hash 15 845f46746f8daf575a42e4bde5cb9d65274e1d98
hunk ./a 1
-
+h
+p M
+i d
*************
hash 33 ca5c07a3d8e20ca553cb6b83d5c2a2df0bd5e34e
hunk ./a 1
+B
+I
+c m
+u
*************
hash 39 7e735b17621d8772e585be32344064c0250455c1
replace ./a [A-Za-z_0-9] f M
^ ^ ^ ^ ^ ^ ^

Sealed2
  (WithStartState2
     (WithNames V1Model [ File "a" [ "" , "" ] ] [])
     (WithSplit
        6
        (ParMS
           (SeqMS
              (ParMS
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "ztsvuthkcyyrmwfyytjn"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 15 845f46746f8daf575a42e4bde5cb9d65274e1d98)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "a" } ])
                                  (Hunk 1 [ "" ] [ "h" , "p M" , "i d" ])
                            } :>:
                          (PrimWithName
                             (PrimPatchId 19 4627349ce0608c332decd5fd80541a3f026cc576)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "a" } ])
                                     (Hunk 4 [ "" ] [ "A T" ])
                               } :>:
                             NilFL))))
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "ntqrzqeyfiehhlogtsuc"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 31 80a23b42177d6d019627de006f92129d6f950654)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "a" } ])
                                  (TokReplace "A-Za-z_0-9" "y" "T")
                            } :>:
                          (PrimWithName
                             (PrimPatchId 66 e9d7ccc116faef824262571d15e15b09dcb68ff9)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "a" } ])
                                     (Hunk 1 [ "" , "" ] [ "Z h" , "W i" ])
                               } :>:
                             NilFL)))))
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "otkvniorrncosojszsxh"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 [ PatchInfo
                     { _piDate = "20240606010532"
                     , _piName = "ztsvuthkcyyrmwfyytjn"
                     , _piAuthor = "tester"
                     , _piLog = []
                     , _piLegacyIsInverted = False
                     }
                 , PatchInfo
                     { _piDate = "20240606010532"
                     , _piName = "ntqrzqeyfiehhlogtsuc"
                     , _piAuthor = "tester"
                     , _piLog = []
                     , _piLegacyIsInverted = False
                     }
                 ]
                 (PrimWithName
                    (PrimPatchId 33 ca5c07a3d8e20ca553cb6b83d5c2a2df0bd5e34e)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (Hunk 1 [] [ "B" , "I" , "c m" , "u" ])
                      } :>:
                    NilFL)))
           (SeqMS
              NilMS
              (NamedP
                 PatchInfo
                   { _piDate = "20240606010532"
                   , _piName = "vqqtzxvvzjgddgwlezwj"
                   , _piAuthor = "tester"
                   , _piLog = []
                   , _piLegacyIsInverted = False
                   }
                 []
                 (PrimWithName
                    (PrimPatchId 39 7e735b17621d8772e585be32344064c0250455c1)
                    Prim
                      { unPrim =
                          FP
                            (AnchoredPath [ Name { unName = "a" } ])
                            (TokReplace "A-Za-z_0-9" "f" "M")
                      } :>:
                    NilFL))))))

(used seed 1452290259292424930)
