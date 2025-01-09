#!/usr/bin/env bash

. lib

rm -rf B
darcs init B
cd B
touch a b
darcs record -lam 'initial state'
cd ..

# 4 branches

rm -rf B1
darcs clone B B1
cd B1
darcs replace J y b
darcs record -am qvcryqkmrjhsjgtmxeus
cat >a <<EOF
M T
i
EOF
echo y | darcs amend --no-canonize -am qvcryqkmrjhsjgtmxeus
cd ..

rm -rf B2
darcs clone B B2
cd B2
mkdir xFR
darcs record -lam tkcsxlftoemrwesezlaa
darcs move xFR izf
echo y | darcs amend --no-canonize -am tkcsxlftoemrwesezlaa
cd ..

rm -rf B3
darcs clone B B3
cd B3
rm b
darcs record -am yxdgxpwxqvkavhgojjrt
cd ..

rm -rf B4
darcs clone B B4
cd B4
mkdir tPS
darcs record -lam btpsqlxpsfelrkidbeyf
cat >a <<EOF
G U
EOF
echo y | darcs amend --no-canonize -am btpsqlxpsfelrkidbeyf
cd ..

rm -rf R1
darcs clone B R1
cd R1
darcs pull -a --allow-conflicts ../B1 -p qvcryqkmrjhsjgtmxeus
darcs pull -a --allow-conflicts ../B3 -p yxdgxpwxqvkavhgojjrt
darcs pull -a --allow-conflicts ../B2 -p tkcsxlftoemrwesezlaa
darcs pull -a --allow-conflicts ../B4 -p btpsqlxpsfelrkidbeyf
# depend qvcryqkmrjhsjgtmxeus
# depend tkcsxlftoemrwesezlaa
# depend btpsqlxpsfelrkidbeyf
echo yynyd | darcs record --ask-deps -m mihrcprtxunbvzhayult
# The purpose of the tag here is to split the repo into
# the uninteresting first 3 patches and the interesting last 2.
# Note that --ask-deps never asks for patches beyond a clean tag
# which is why we have to first record mihrcprtxunbvzhayult and
# then do the obliterate/tag/re-apply sequence here.
echo y | darcs obliterate -a --last=2 -o saved_patches
darcs tag first_three
darcs apply saved_patches --allow-conflicts
darcs mark-conflicts >log 2>&1
not darcs whatsnew >>log
cd ..

rm -rf R2
darcs clone B R2
cd R2
darcs pull -a --allow-conflicts ../R1 -p yxdgxpwxqvkavhgojjrt
darcs pull -a --allow-conflicts ../R1 -p qvcryqkmrjhsjgtmxeus
darcs pull -a --allow-conflicts ../R1 -p tkcsxlftoemrwesezlaa
darcs pull -a ../R1 -p first_three
darcs pull -a --allow-conflicts ../R1 -p btpsqlxpsfelrkidbeyf
darcs pull -a --allow-conflicts ../R1 -p mihrcprtxunbvzhayult
darcs mark-conflicts >log 2>&1
not darcs whatsnew >>log
cd ..

diff -u R1/log R2/log >&2

exit; # success

Named RepoPatchV3:
  using V2.Prim wrapper for Prim.V1:
    resolutions are invariant under reorderings: [Failed]
*** Failed! (after 14185 tests and 10 shrinks):
resolutions differ: r1=

[]

r2=

[ [ Sealed
      (Prim
         { unPrim =
             FP (AnchoredPath [ Name { unName = "a" } ]) (Hunk 1 [] [ "G U" ])
         } :>:
         NilFL)
  , Sealed
      (Prim
         { unPrim =
             FP
               (AnchoredPath [ Name { unName = "a" } ])
               (Hunk 1 [] [ "M T" , "i" ])
         } :>:
         NilFL)
  ]
]

for context

patch 6852635f0e39e7a8563e00bdba8cdc1b834da65a
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * qvcryqkmrjhsjgtmxeus
hash 54 026a0ba3a52003aade8ccbd1350455b84d4f3b42
replace ./b [A-Za-z_0-9] J y
hash 65 a50456d6e0f32f49e1dfffb9e1672582ef1dd236
hunk ./a 1
+M T
+i
patch 648169e7afba6bd48b0fe7eab3eb1a352ebd4535
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * yxdgxpwxqvkavhgojjrt
conflictor
hash -54 026a0ba3a52003aade8ccbd1350455b84d4f3b42
replace ./b [A-Za-z_0-9] y J
v v v v v v v
hash 54 026a0ba3a52003aade8ccbd1350455b84d4f3b42
replace ./b [A-Za-z_0-9] J y
*************
hash 52 4699828c612aba576e7482d89e818eedcc7bf6c2
rmfile ./b
^ ^ ^ ^ ^ ^ ^
patch c1e2b8d9b4ee5e7a604c7fc300ffd8da2500d7ed
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * tkcsxlftoemrwesezlaa
hash 73 5196376fbf92761a84841970cb492813aa19b3a5
adddir ./xFR
hash 83 b41dd5ff02370ea774981eb18c3bb2b0c680f8b0
move ./xFR ./izf

and patches

patch 11ff78b7c98145880da912fc1d5f5e69c7acbb10
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * btpsqlxpsfelrkidbeyf
hash 21 29334fc3358bad8f40554d902f1af6d0b9df948d
adddir ./tPS
conflictor
hash -65 a50456d6e0f32f49e1dfffb9e1672582ef1dd236
hunk ./a 1
-M T
-i
v v v v v v v
hash 65 a50456d6e0f32f49e1dfffb9e1672582ef1dd236
hunk ./a 1
+M T
+i
*************
hash 55 db8bb25ec02832bc28ab3b9dbd9353dbf2b3c270
hunk ./a 1
+G U
^ ^ ^ ^ ^ ^ ^
patch ab3674a9ebc11889747f0a29e076c5230e56dbf6
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * mihrcprtxunbvzhayult
depend 6852635f0e39e7a8563e00bdba8cdc1b834da65a
  * qvcryqkmrjhsjgtmxeus
depend c1e2b8d9b4ee5e7a604c7fc300ffd8da2500d7ed
  * tkcsxlftoemrwesezlaa
depend 11ff78b7c98145880da912fc1d5f5e69c7acbb10
  * btpsqlxpsfelrkidbeyf

versus

for context

patch 648169e7afba6bd48b0fe7eab3eb1a352ebd4535
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * yxdgxpwxqvkavhgojjrt
hash 52 4699828c612aba576e7482d89e818eedcc7bf6c2
rmfile ./b
patch 6852635f0e39e7a8563e00bdba8cdc1b834da65a
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * qvcryqkmrjhsjgtmxeus
conflictor
hash -52 4699828c612aba576e7482d89e818eedcc7bf6c2
addfile ./b
v v v v v v v
hash 52 4699828c612aba576e7482d89e818eedcc7bf6c2
rmfile ./b
*************
hash 54 026a0ba3a52003aade8ccbd1350455b84d4f3b42
replace ./b [A-Za-z_0-9] J y
^ ^ ^ ^ ^ ^ ^
hash 65 a50456d6e0f32f49e1dfffb9e1672582ef1dd236
hunk ./a 1
+M T
+i
patch c1e2b8d9b4ee5e7a604c7fc300ffd8da2500d7ed
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * tkcsxlftoemrwesezlaa
hash 73 5196376fbf92761a84841970cb492813aa19b3a5
adddir ./xFR
hash 83 b41dd5ff02370ea774981eb18c3bb2b0c680f8b0
move ./xFR ./izf

and patches

patch 11ff78b7c98145880da912fc1d5f5e69c7acbb10
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * btpsqlxpsfelrkidbeyf
hash 21 29334fc3358bad8f40554d902f1af6d0b9df948d
adddir ./tPS
conflictor
hash -65 a50456d6e0f32f49e1dfffb9e1672582ef1dd236
hunk ./a 1
-M T
-i
v v v v v v v
hash 65 a50456d6e0f32f49e1dfffb9e1672582ef1dd236
hunk ./a 1
+M T
+i
*************
hash 55 db8bb25ec02832bc28ab3b9dbd9353dbf2b3c270
hunk ./a 1
+G U
^ ^ ^ ^ ^ ^ ^
patch ab3674a9ebc11889747f0a29e076c5230e56dbf6
Author: tester
Date:   Thu Jun  6 03:05:32 CEST 2024
  * mihrcprtxunbvzhayult
depend 6852635f0e39e7a8563e00bdba8cdc1b834da65a
  * qvcryqkmrjhsjgtmxeus
depend c1e2b8d9b4ee5e7a604c7fc300ffd8da2500d7ed
  * tkcsxlftoemrwesezlaa
depend 11ff78b7c98145880da912fc1d5f5e69c7acbb10
  * btpsqlxpsfelrkidbeyf

Sealed2
  (WithStartState2
     (WithNames V1Model [ File "a" [] , File "b" [] ] [])
     (WithSplit
        2
        (SeqMS
           (ParMS
              (ParMS
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "qvcryqkmrjhsjgtmxeus"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 54 026a0ba3a52003aade8ccbd1350455b84d4f3b42)
                          Prim
                            { unPrim =
                                FP
                                  (AnchoredPath [ Name { unName = "b" } ])
                                  (TokReplace "A-Za-z_0-9" "J" "y")
                            } :>:
                          (PrimWithName
                             (PrimPatchId 65 a50456d6e0f32f49e1dfffb9e1672582ef1dd236)
                             Prim
                               { unPrim =
                                   FP
                                     (AnchoredPath [ Name { unName = "a" } ])
                                     (Hunk 1 [] [ "M T" , "i" ])
                               } :>:
                             NilFL))))
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "tkcsxlftoemrwesezlaa"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 73 5196376fbf92761a84841970cb492813aa19b3a5)
                          Prim
                            { unPrim = DP (AnchoredPath [ Name { unName = "xFR" } ]) AddDir
                            } :>:
                          (PrimWithName
                             (PrimPatchId 83 b41dd5ff02370ea774981eb18c3bb2b0c680f8b0)
                             Prim
                               { unPrim =
                                   Move
                                     (AnchoredPath [ Name { unName = "xFR" } ])
                                     (AnchoredPath [ Name { unName = "izf" } ])
                               } :>:
                             NilFL)))))
              (ParMS
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "yxdgxpwxqvkavhgojjrt"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 52 4699828c612aba576e7482d89e818eedcc7bf6c2)
                          Prim
                            { unPrim = FP (AnchoredPath [ Name { unName = "b" } ]) RmFile } :>:
                          NilFL)))
                 (SeqMS
                    NilMS
                    (NamedP
                       PatchInfo
                         { _piDate = "20240606010532"
                         , _piName = "btpsqlxpsfelrkidbeyf"
                         , _piAuthor = "tester"
                         , _piLog = []
                         , _piLegacyIsInverted = False
                         }
                       []
                       (PrimWithName
                          (PrimPatchId 21 29334fc3358bad8f40554d902f1af6d0b9df948d)
                          Prim
                            { unPrim = DP (AnchoredPath [ Name { unName = "tPS" } ]) AddDir
                            } :>:
                          (PrimWithName
                             (PrimPatchId 55 db8bb25ec02832bc28ab3b9dbd9353dbf2b3c270)
                             Prim
                               { unPrim =
                                   FP (AnchoredPath [ Name { unName = "a" } ]) (Hunk 1 [] [ "G U" ])
                               } :>:
                             NilFL))))))
           (NamedP
              PatchInfo
                { _piDate = "20240606010532"
                , _piName = "mihrcprtxunbvzhayult"
                , _piAuthor = "tester"
                , _piLog = []
                , _piLegacyIsInverted = False
                }
              [ PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "qvcryqkmrjhsjgtmxeus"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              , PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "tkcsxlftoemrwesezlaa"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              , PatchInfo
                  { _piDate = "20240606010532"
                  , _piName = "btpsqlxpsfelrkidbeyf"
                  , _piAuthor = "tester"
                  , _piLog = []
                  , _piLegacyIsInverted = False
                  }
              ]
              NilFL))))

(used seed 5556232291165384773) -q=100000
