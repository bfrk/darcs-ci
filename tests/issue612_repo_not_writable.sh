#!/usr/bin/env bash

# Test that darcs fails appropriately when the target repo inventory file is
# not writable. See issue612

. lib

# We can set and clear permission bits with bash on Windows but that
# has not the expected effect on programs.
abort_windows

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
touch t.t
darcs add t.t
darcs record -am "initial add"
trap "chmod -R +w $(pwd)/_darcs/inventories" EXIT
chmod -R a-w _darcs/inventories
cd ..

darcs get temp1 temp2
cd temp2
echo new >> t.t
darcs record -am "new patch"
not darcs push -a ../temp1
cd ..
