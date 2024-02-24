#!/usr/bin/env bash

# tests for "darcs mark-conflicts"

. lib

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
echo "Conflict, Base ." > child_of_conflict
darcs add child_of_conflict
darcs record -am 'Conflict Base'
cd ..
darcs get temp1 temp2

# Add and record differing lines to both repos
cd temp1
echo "Conflict, Part 1." > child_of_conflict
darcs record -A author -am 'Conflict Part 1'
cd ..
cd temp2
echo "Conflict, Part 2." > child_of_conflict
darcs record -A author -am 'Conflict Part 2'
cd ..

cd temp1
darcs pull -a ../temp2 >log 2>&1
grep conflict log
grep -i finished log
grep 'v v' child_of_conflict
darcs revert -a
not grep 'v v' child_of_conflict
darcs mark-conflicts
grep 'v v' child_of_conflict

# test that a change depending on both parts counts as resolution,
# so that mark-conflicts doesn't see any conflicts to mark
darcs revert  -a
echo "Conflict, Part 3." > child_of_conflict
# it should count as resolution regardless of whether unrecorded...
darcs mark-conflicts | grep -i 'No conflicts'
not grep 'v v' child_of_conflict
# ...or recorded
darcs record -am 'Conflict resolution'
darcs mark-conflicts | grep -i 'No conflicts'
not grep 'v v' child_of_conflict

cd ..
