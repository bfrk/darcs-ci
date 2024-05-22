#!/usr/bin/env bash

# amend --unrecord should move unrecorded changes to pending

. lib

rm -rf R
darcs init R
cd R

echo bla > bla
darcs record -lam 'bla'
echo yyy | darcs amend --unrecord -a bla
darcs whatsnew > new
grep 'addfile ./bla' new

cd ..
