#!/usr/bin/env bash

## Test that we produce exactly correct output when sending
##
## Copyright (C) 2010 Ganesh Sittampalam
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib

case $format in
  darcs-1)
    v=v1
    ;;
  darcs-2)
    v=v2
    ;;
  darcs-3)
    v=v3
    ;;
esac

rm -rf empty
mkdir empty
cd empty
darcs init
cd ..

rm -rf repo
unpack_testdata simple-$v
cd repo
darcs send --no-minimize -o repo.dpatch -a ../empty

compare_bundles $TESTDATA/simple-$v.dpatch repo.dpatch
cd ..

# test that we are including some context lines in hunk patches
rm -rf repo
unpack_testdata context-$v
cd repo
darcs send --no-minimize -o repo.dpatch -a ../empty
compare_bundles $TESTDATA/context-$v.dpatch repo.dpatch
cd ..
