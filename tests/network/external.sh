#!/usr/bin/env bash

# Some tests for launching external commands

. lib

rm -rf temp1
rm -f fakessh

fakessh=./fakessh
echo '#!/bin/sh' > $fakessh
echo 'echo hello > touchedby_fakessh' >> $fakessh
chmod u+x $fakessh

export DARCS_SSH=$fakessh
export DARCS_SCP=$fakessh
export DARCS_SFTP=$fakessh

# first test the DARCS_SSH environment variable
rm -rf touchedby_fakessh
not darcs clone example.com:foo
grep hello touchedby_fakessh

# now make sure that we don't launch ssh for nothing
rm -f touchedby_fakessh
mkdir temp1
cd temp1
darcs init
cd ..
darcs clone temp1 > log
not grep touchedby_fakessh log
not darcs clone http://darcs.net/nonexistent
not grep touchedby_fakessh log
cd ..
