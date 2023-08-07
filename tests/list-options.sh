#!/usr/bin/env bash
# some tests for the --list-options option

. lib

rm -rf R
darcs init R
cd R
echo aboringfile > _darcs/prefs/boring
touch anunaddedfile
touch aboringfile
darcs record --list-options | not grep -w anunaddedfile
darcs record -l --list-options | grep -w anunaddedfile
darcs record -l --list-options | not grep -w aboringfile
darcs record --boring --list-options | grep -w anunaddedfile
darcs record --boring --list-options | grep -w aboringfile
darcs add --list-options | grep -w anunaddedfile
darcs add --list-options | not grep -w aboringfile
darcs add --boring --list-options | grep -w anunaddedfile
darcs add --boring --list-options | grep -w aboringfile
cd ..
