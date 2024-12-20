#!/usr/bin/env bash
# 2011, by Petr Rockai, Guillaume Hoffmann, public domain

# Tests that darcs clone --verbose reports getting a pack when there is one,
# and does not report when there is none or when --no-packs is passed.

. lib
. httplib

only-format darcs-2 # compressed repo is darcs-2

gunzip -c $TESTDATA/laziness-complete.tgz | tar xf -

cd repo

darcs optimize http
test -e _darcs/packs/basic.tar.gz
test -e _darcs/packs/patches.tar.gz
cd ..

serve_http # sets baseurl

# check that default behaviour is to get packs
rm -rf S
darcs clone $baseurl/repo S --verbose | grep "Trying to clone packed basic repository"

# check that it does really not get packs when --no-packs is passed
rm -rf S
darcs clone $baseurl/repo S --no-packs --verbose | not grep "Trying to clone packed basic repository"

# check that it does not claim getting packs when there are not
rm -rf S
rm -rf repo/_darcs/packs/
# sleep for a second to avoid spurious false positives on MacOS:
sleep 1
darcs clone $baseurl/repo S --verbose | grep "Remote repo has no basic pack"
