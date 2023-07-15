#!/usr/bin/env bash

# Test that darcs fails appropriately when the target repo inventory file is not writable.
# See issue612

. lib

#abort_windows

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
touch t.t
darcs add t.t
darcs record -am "initial add"
if [ -e _darcs/inventories ]; then
  chmod a-w _darcs/inventories/*
  chmod a-w _darcs/inventories
fi
if [ -e _darcs/inventory ]; then
  chmod a-w _darcs/inventory
fi
cd ..

darcs get temp1 temp2
cd temp2
# this block may fail so we'd better make sure we clean up after
# ourselves to avoid a permissions mess for other tests
trap "cd ..; chmod -R +w temp1" EXIT
echo new >> t.t
darcs record -am "new patch"
not darcs push -a ../temp1 2> log
grep failed log

