#!/usr/bin/env bash
## Test for issue2312 - 
##     posthooks for 'record' and 'amend-record' should receive DARCS_PATCHES
##
## Copyright (C) 2013 Sebastian Fischer
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

rm -rf R
mkdir R
cd R
darcs init

touch some.file
darcs add some.file

# posthook for darcs record should receive DARCS_PATCHES with correct change
darcs record -am msg1 --posthook="printenv DARCS_PATCHES" > out
cat out
grep msg1 out
grep "A ./some.file" out

# posthook for amend-record should receive DARCS_PATCHES with correct change
echo contents > some.file
echo y | darcs amend-record -a --posthook="printenv DARCS_PATCHES" > out
cat out
grep msg1 out
grep "A ./some.file" out

# newly added file should appear after amend
echo more contents >> some.file
touch new.file
darcs record -am msg2 --posthook="printenv DARCS_PATCHES" > out
cat out
grep msg2 out
grep "M ./some.file" out
not grep "A ./new.file" out
darcs add new.file
echo y | darcs amend-record -a --posthook="printenv DARCS_PATCHES" > out
cat out
not grep msg1 out
grep msg2 out
grep "M ./some.file" out
grep "A ./new.file" out

# no change should appear if it is not recorded
echo > out      # clear out file, in case posthook is not called
echo contents >> new.file
echo ny | darcs record -m msg3 --posthook="printenv DARCS_PATCHES" > out
not grep msg1 out
not grep msg2 out
not grep msg3 out
not grep "M ./new.file" out

cd ..
