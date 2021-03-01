#!/usr/bin/env bash
# issue2668 is actually two separate issues:
# * patch index does not work with repos whose parent dir is read-only
# * failure to create or update patch index should not make other commands fail

. lib

readonly_path=$(/bin/pwd)/readonly
trap "chmod -R +w $readonly_path" EXIT

rm -rf readonly
mkdir readonly

darcs init R

darcs init S
darcs optimize enable-patch-index --repo=S

mv R S readonly
chmod -w readonly

# 1st problem
cd readonly/R
darcs optimize disable-patch-index
darcs optimize enable-patch-index
cd ../..

# 2nd problem
cd readonly/S
# provoke failure when we try to update the patch index
# even if 1st problem is no longer an issue
chmod -w _darcs/patch_index
echo text > file
darcs record -l file -am 'file'
darcs unrecord -a -p 'file'
cd ../..
