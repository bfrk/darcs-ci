#!/usr/bin/env bash

. lib

darcs init R
cd R
# need to have at least one patch as long as we throw the error
# only when we actually try to match
touch f
darcs rec -lam 'dummy'
# test that using an invalid regex is not treated as bug in darcs
not darcs log -p '' >&2
not darcs log -p '[' >&2
not darcs log -p '*x' >&2
cd ..
