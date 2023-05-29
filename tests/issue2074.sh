#!/usr/bin/env bash

. lib

darcs init R
cd R
touch a
darcs rec -l a -am 'a'
echo "line1" > a
darcs rev -a
# the "n" cancels obliterate after the
# "this will make unrevert impossible" question
# which is not a failure
echo "yyn" | darcs ob
cd ..
