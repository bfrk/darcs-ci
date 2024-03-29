#!/usr/bin/env bash
. ./lib

# test fails for these obsolete formats:
skip-formats darcs-1 darcs-2

# See issue1327.
# results in the error:
# patches to commute_to_end does not commutex (1) at src/Darcs/Patch/Depends.hs:452


rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
echo fileA version 1 > fileA
echo fileB version 1 > fileB
darcs add fileA fileB
darcs record --author foo@bar --all -m "Add fileA and fileB"
echo fileA version 2 > fileA
darcs record --author foo@bar --all -m "Modify fileA"
cd ..
darcs get temp1 temp2
cd temp2
darcs obliterate -p "Modify fileA" --all
darcs unrecord -p "Add fileA and fileB" --all
darcs record --author foo@bar --all fileA -m "Add just fileA"
cd ../temp1
darcs pull --all ../temp2
echo yy | darcs obliterate --dont-prompt-for-dependencies -p "Add fileA and fileB"
