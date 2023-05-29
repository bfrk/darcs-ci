#!/usr/bin/env bash

. lib

darcs init R
cd R
# test that using an invalid regex is not treated as bug in darcs
not darcs log -p '' >&2
not darcs log -p '[' >&2
not darcs log -p '*x' >&2
cd ..
