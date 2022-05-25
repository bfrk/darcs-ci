#!/usr/bin/env bash
## Test for issue1599 - 'Automatically expire unused caches'
##
## Copyright (C) 2010  Adolfo Builes
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
. httplib

rm -rf R S log && mkdir R
cd R
darcs init
echo a > a
darcs rec -lam a
echo b > b
darcs rec -lam b
echo c > c
darcs rec -lam c
darcs tag bla # so that the clone really is lazy
cd ..

serve_http # sets baseurl
darcs clone --lazy $baseurl/R S
rm S/_darcs/prefs/sources
if [ -z "$http_proxy" ]; then
  echo "repo:http://10.1.2.3/S" >> S/_darcs/prefs/sources
fi
echo "repo:$baseurl/dummyRepo" >> S/_darcs/prefs/sources
echo "repo:/does/not/exist" >> S/_darcs/prefs/sources
echo "repo:$baseurl/R" >> S/_darcs/prefs/sources
DARCS_CONNECTION_TIMEOUT=1 darcs log --repo S --debug --no-cache 2>&1 | tee log
grep "could not reach the following locations" log
# the count is two, once for the file we want, and once to test
# for reachability using _darcs/hashed_inventory
if test -z "$http_proxy"; then
  test `grep -c "copyRemote: http://10.1.2.3/S" log` = 2
fi
test `grep -c "copyRemote: $baseurl/dummyRepo" log` = 2
