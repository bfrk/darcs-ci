#!/usr/bin/env bash

. lib

rm -rf R
darcs init R
cd R
dd if=/dev/zero of=large bs=1000000 count=2200
# check that it does not fail with an IOC exception
# but just because it runs into a parse error
not darcs apply 2>&1 | grep 'not enough input'
cd ..
