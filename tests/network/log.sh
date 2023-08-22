#!/usr/bin/env bash

. lib
. httplib

check_remote_http https://hub.darcs.net/darcs/darcs-screened

# Demonstrates issue385 and others
sleep 1
darcs log --repo=https://hub.darcs.net/darcs/darcs-screened GNUmakefile --last 30

# Test things mentioned in issue2461:

# no _darcs should remain
test ! -d _darcs

# go to a directory where we have no write access
trap "chmod u+w $PWD/ro" EXIT
mkdir ro
chmod a-w ro
cd ro
sleep 1
# and try again (with less patches to fetch)
darcs log --repo=https://hub.darcs.net/darcs/darcs-screened GNUmakefile --last 3
sleep 1
# an absolute path should give an error
not darcs log --repo=https://hub.darcs.net/darcs/darcs-screened /GNUmakefile --last 3
sleep 1
# also test that it works without any filename arguments
darcs log --repo=https://hub.darcs.net/darcs/darcs-screened --last 1
sleep 1
cd ..
