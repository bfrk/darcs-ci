#!/usr/bin/env bash
. ./lib

# tests for "darcs optimize"

## test that darcs optimize reorder works

rm -rf test1 test1a
mkdir test1
cd test1
darcs init
touch foo
darcs record -a -m add_foo -l foo
darcs tag foo_tag
# check tag is initially clean
grep 'Starting with inventory' _darcs/hashed_inventory
touch bar
darcs record -a -m add_bar -l bar
# make the tag unclean
echo y | darcs amend -p foo_tag -a --author me
not grep 'Starting with inventory' _darcs/hashed_inventory
# save repo for next test
darcs clone . ../test1a
# the actual test
darcs optimize reorder | grep -i "done"
# check it is again clean
grep 'Starting with inventory' _darcs/hashed_inventory
cd ..

## optimize reorder --deep

cd test1a
# we have an unclean tag foo_tag; add another tag
darcs tag bar_tag
darcs optimize reorder | grep -i "done"
# this makes bar_tag clean:
# neither foo_tag nor add_foo are in the head inventory
not grep add_foo _darcs/hashed_inventory
not grep foo_tag _darcs/hashed_inventory
# but foo_tag remains dirty
# (this greps for a lone inventory hash)
grep -E '^[0-9]+-[0-9a-f]+$' _darcs/hashed_inventory > ihash
zgrep add_foo _darcs/inventories/$(cat ihash)
# now do the deep reorder
darcs optimize reorder --deep
# add_foo is not in the parent inventory
grep -E '^[0-9]+-[0-9a-f]+$' _darcs/hashed_inventory > ihash
not zgrep add_foo _darcs/inventories/$(cat ihash)
# but instead in the grandparent
zgrep -E '^[0-9]+-[0-9a-f]+$' _darcs/inventories/$(cat ihash) > ihash2
zgrep add_foo _darcs/inventories/$(cat ihash2)
cd ..

## issue2388 - optimize fails if no patches have been recorded

rm -rf test2
darcs init test2
cd test2
darcs optimize clean
cd ..
