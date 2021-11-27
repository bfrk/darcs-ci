#!/usr/bin/env bash

. ./lib

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1
darcs init
mkdir dir
darcs record -a -m add_dir -A x --look-for-adds
not darcs whatsnew
echo zig > dir/foo
echo zag > foo
mkdir dir2
echo hi > dir2/foo2
darcs record -a -m add_foo -A x --look-for-adds
not darcs whatsnew
cd ../temp2
darcs init
darcs pull -a ../temp1
cd ..
cmp temp1/dir2/foo2 temp2/dir2/foo2
cmp temp1/dir/foo temp2/dir/foo
cmp temp1/foo temp2/foo
rm -rf temp1 temp2

