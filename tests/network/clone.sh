#!/usr/bin/env bash

. lib
. httplib

rm -rf tabular
unpack_testdata tabular
serve_http # sets baseurl

rm -rf temp2 temp3
darcs clone --lazy $baseurl/tabular temp2
darcs clone --lazy --tag . $baseurl/tabular temp3

cd temp2
darcs obliterate --from-tag . -a
darcs pull --tag . -a
cd ..

diff -u temp2/_darcs/hashed_inventory temp3/_darcs/hashed_inventory
