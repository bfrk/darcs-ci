#!/usr/bin/env bash
## Test for issue154 - when applying a patch that removes a directory,
## don't remove the directory from the working tree unless it's empty.
##
## Copyright (C) 2008  Mark Stosberg
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

rm -rf R S
darcs init R
cd R
mkdir d
darcs add d
darcs record -a -m "Added directory d"
darcs get . ../S
cd ../S
touch d/moo
darcs add d/moo
cd ../R
rm -rf d
darcs record -a -m "Remove directory d"
cd ../S
echo y | darcs pull -a ../R 2>&1 > log
grep -i "backing up" log
grep -i "finished pulling" log
# check that moo is not lost (it will be in a backup of d)
find . -name moo
# ideally we would preserve the pending 'addfile ./d/moo',
# but we currently do not
#darcs whatsnew | grep 'addfile ./d/moo'
darcs whatsnew | grep 'adddir ./d'
cd ..
