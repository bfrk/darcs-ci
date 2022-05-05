#!/usr/bin/env bash

. ./lib

rm -rf temp1

mkdir temp1
cd temp1
darcs init
echo -e 'foo\nbar\nbaz' > f
darcs rec -Ax -alm p1
echo -e 'foo\nbar\nwibble' > f
darcs rec -Ax -alm p2
echo -e 'baz\nbar\nwibble' > f

# check that wh (without --ignore-times) sees the change
darcs wh > whatsnew
grep 'foo' whatsnew

# the (once) problematic unpull
darcs unpull --last 1 -a

# whatsnew no longer thinks there are no changes without --ignore-times
darcs wh > whatsnew
grep 'foo' whatsnew

cd ..
rm -rf temp1
