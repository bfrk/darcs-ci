#!/usr/bin/env bash
. ./lib

rm -rf tempOld tempA tempB
mkdir tempOld tempA
cd tempOld
darcs initialize
echo record author me > _darcs/prefs/defaults
echo ALL all >> _darcs/prefs/defaults
#echo ALL verbose >> _darcs/prefs/defaults
echo A > foo
echo B >> foo
echo C >> foo
echo D >> foo
echo E >> foo
echo F >> foo
echo G >> foo
echo H >> foo
darcs add foo
darcs record -m Old
cd ..

cd tempA
darcs initialize
cp ../tempOld/_darcs/prefs/defaults _darcs/prefs
darcs pull ../tempOld
cp foo temp
cat temp | grep -v A | grep -v B | grep -v D | sed s/E/e/ \
    | grep -v G | sed s/H/h/ > foo
darcs record -m AA
cd ..

darcs get tempOld tempB
cd tempB
cp ../tempOld/_darcs/prefs/defaults _darcs/prefs
echo 7 > foo
darcs record -m BB
darcs pull ../tempA
darcs record -m "conflict resolution"
cd ..

cd tempA
darcs pull ../tempB
darcs log -v --max-count 1 -p B | cat
darcs log -v --max-count 1 -p resolution | cat
cd ..

cmp tempA/foo tempB/foo

rm -rf tempOld tempA tempB

