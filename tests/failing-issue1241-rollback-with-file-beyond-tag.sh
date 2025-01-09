#!/usr/bin/env bash

. lib

rm -rf R
darcs init R
cd R
echo one > f
darcs record -lam one
darcs tag tag
# this works as expected:
darcs rollback -a --matches 'touch f' >&2
darcs whatsnew >&2
darcs revert -a
# but this says 'No patches selected':
darcs rollback -a f >&2
darcs whatsnew >&2
cd ..
