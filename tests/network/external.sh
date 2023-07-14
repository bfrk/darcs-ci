#!/usr/bin/env bash

# Some tests for launching external commands

. lib

rm -rf foo temp1 temp2
rm -f fakessh

fakessh=$(pwd)/fakessh
cat >$fakessh.hs <<EOF
main = writeFile "touchedby_fakessh" "hello\n"
EOF
ghc --make $fakessh.hs

export DARCS_SSH=$fakessh
export DARCS_SCP=$fakessh
export DARCS_SFTP=$fakessh

# first test the DARCS_SSH environment variable
rm -f touchedby_fakessh
darcs clone example.com:foo
grep hello touchedby_fakessh

# now make sure that we don't launch ssh for nothing
rm -f touchedby_fakessh
darcs init temp1
darcs clone temp1 temp2 > log
not grep touchedby_fakessh log
not darcs clone http://darcs.net/nonexistent
not grep touchedby_fakessh log
