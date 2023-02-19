#!/usr/bin/env bash
##
## Checking what happens when we need to remove an add from pending
## after doing a move.
##
## Copyright (C) 2018 Ganesh Sittampalam
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

rm -rf temp1
mkdir temp1
cd temp1
darcs init

echo 'foo' > a
echo 'bar' > b

darcs add a b

mv a c

darcs rec --look-for-moves c -a -m"added c via a"

darcs whatsnew > got
cat >want <<EOF
addfile ./b
hunk ./b 1
+bar
EOF

diff -u want got >&2

cd ..


rm -rf temp2
mkdir temp2
cd temp2
darcs init

echo 'foo' > a
echo 'bar' > b

darcs add a b

mv b c

darcs rec --look-for-moves c -a -m"added c via b"

darcs whatsnew > got
cat >want <<EOF
addfile ./a
hunk ./a 1
+foo
EOF

diff -u want got >&2

cd ..

# Now test what happens if we do /not/ select the coalesced add+move

rm -rf temp3
mkdir temp3
cd temp3
darcs init

echo 'foo' > a
echo 'bar' > b

darcs add a b

mv b c

darcs whatsnew --look-for-moves > got
cat >want <<EOF
addfile ./a
hunk ./a 1
+foo
addfile ./c
hunk ./c 1
+bar
EOF

echo yyny | darcs rec --look-for-moves -m"added a (but not c via b)"

# make sure we recorded only changes to ./a
darcs log -v | grep -vF ./b | grep -vF ./c

# check pending still has the addfile ./b
grep -F 'addfile ./b' _darcs/patches/pending
# but not the detected move
grep -v move _darcs/patches/pending

# The tests below are disable because --look-for-moves does not detect
# the move in this case. It is currently (2022-07-21) unclear to me why.

if false; then
darcs whatsnew --look-for-moves > got
cat >want <<EOF
addfile ./c
hunk ./c 1
+bar
EOF

diff -u want got >&2

darcs whatsnew > got
cat >want <<EOF
addfile ./c
hunk ./c 1
+bar
EOF

diff -u want got >&2
fi

cd ..

# The same thing backwards, that is, we first `darcs move`,
# then remove the target w/o telling darcs
# then record only the (forced) hunk and not the
# coalesced move+rmfile.
# The expectation is that pending still contains
# the move, but not the coalesced "rmfile ./a".

rm -rf temp4
mkdir temp4
cd temp4
darcs init

echo 'foo' > a

darcs add a
darcs record -am 'add a with content'

darcs move a b
rm b

# remember the pending patch
cp _darcs/patches/pending pending.before

darcs whatsnew > dwh
# coalescing means dwh should be:
cat >dwh.expected <<EOF
hunk ./a 1
-foo
rmfile ./a
EOF
# check that
diff -u dwh.expected dwh >&2

# record only the hunk
echo yny | darcs rec -m"only the hunk"

# The actual test:
# Since whatsnew always "looks for removes" we can't use it here;
# instead check that _darcs/patches/pending hasn't changed:
diff -u pending.before _darcs/patches/pending >&2

cd ..
