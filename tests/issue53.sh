#!/usr/bin/env bash

. lib

# pull from not empty repo to empty repo
rm -rf temp1
mkdir temp1

cd temp1
darcs init
echo a > Aux.hs
not darcs add Aux.hs
if ! os_is_windows; then
  darcs add --reserved-ok Aux.hs
fi
echo b > foo
darcs add foo
darcs record -am 'two files'
not darcs mv foo com1
if ! os_is_windows; then
  darcs mv --reserved-ok foo com1
fi
cd ..
