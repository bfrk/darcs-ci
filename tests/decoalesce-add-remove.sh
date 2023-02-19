#!/usr/bin/env bash

. lib

rm -rf R
darcs init R
cd R

touch f
touch g
darcs add f
darcs add g
rm g

grep "addfile ./g" _darcs/patches/pending

# record only f
darcs record -a -m 'add f'
# check that we did not record anything about g
darcs log -v | tee log >&2
not grep -F ./g log

# check that we still have a pending "addfile g"
# even if on the fly we coalesced it with the rm
# in the working tree to NilFL

grep "addfile ./g" _darcs/patches/pending
touch g
darcs whatsnew | grep "addfile ./g"

cd ..
