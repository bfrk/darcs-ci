#!/usr/bin/env bash
. ./lib

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1
darcs init
echo -n zig > foo
darcs add foo
darcs record -a -m add_foo -A x
echo -n zag >> foo
darcs record --ignore-time -a -m mod_foo -A x
cd ../temp2
darcs init
darcs pull -a ../temp1
cd ..
cmp temp1/foo temp2/foo
rm -rf temp1 temp2

