#!/usr/bin/env bash
## Test for issue1857 - upgrading the pristine format should either
## work or have no effect) even it happens before a failing operation
##
## Copyright (C) 2010 Eric Kow
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

. lib                           # Load some portability helpers.
rm -rf minimal-darcs-2.4
unpack_testdata minimal-darcs-2_4

cd minimal-darcs-2.4
darcs check
# we used to do
#  darcs setpref test false
# here but that will now do the pristine conversion itself
# (during revertRepositoryChanges), so we have to fake it:
echo 'test false' >> _darcs/prefs/prefs
echo 'hi' > README
not darcs record -a -m argh --test 2> errlog 1> outlog
# check that we are really doing the pristine conversion...
grep -i 'converting pristine' errlog
# ...and not fail for some other reason
grep "Test failed" outlog
darcs check
cd ..
