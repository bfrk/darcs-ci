#!/usr/bin/env bash

. lib

rm -rf R
darcs init R
cd R

touch f
darcs record -lam 'add f'

touch g
darcs add g
darcs replace one two f
rm f

echo nyy | darcs record -m 'add g'

# make sure we recorded the addfile ./g but nothing about f:
darcs log --last=1 -v > log
grep -F 'addfile ./g' log
grep -v -F './f' log

# test that pending still contains the replace that was eliminated
# (in memory) by coalescing it with the detected rm
cat >pending.want <<EOF
replace ./f [A-Za-z_0-9] one two
EOF

diff -u pending.want _darcs/patches/pending >&2

cd ..
