#!/usr/bin/env bash

. lib

rm -rf R
darcs init R
cd R

mkdir d
mkdir e
touch d/f
touch e/g
darcs add d/f
darcs add e/g
rm -r e

grep "addfile ./e/g" _darcs/patches/pending

# record only d/f
darcs record -a -m 'add d/f'
# check that we did not record anything about g
darcs log -v | tee log >&2
not grep -F ./g log

# check that we still have a pending "addfile g"
# even if on the fly we coalesced it with the rm
# in the working tree to NilFL

grep "addfile ./e/g" _darcs/patches/pending
mkdir e
touch e/g
darcs whatsnew | grep "addfile ./e/g"

cd ..
