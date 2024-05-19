#!/usr/bin/env bash
## Test for issueNNNN - <SYNOPSIS: WHAT IS THE BUG?  THIS SYNOPSIS
## SHOULD BE ONE OR TWO SENTENCES.>
##
## Copyright (C) YEAR  AUTHOR
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

# every test script MUST source lib
. lib

# every test script should remove directories before creating them
rm -rf R S
darcs init R
darcs init S

cat <<EOF > xxx.hs
import System.Directory
main = copyFile "nonexistent" "shouldnotexist"
EOF
ghc xxx.hs
not ./xxx && not test -e "shouldnotexist"

cd R
# change the working tree
mkdir d e
echo 'Example content.' > d/f
echo "content of _darcs/rebase:" >&2
test -e _darcs/rebase && cat _darcs/rebase >&2
darcs record -lam 'Add d/f and e.' --debug
darcs mv d/f e/
darcs record -am 'Move d/f to e/f.'
# push patches from R to S
darcs push ../S -a
cd ..

cd S
# push patches back from S to R
darcs push ../R -a
cd ..
