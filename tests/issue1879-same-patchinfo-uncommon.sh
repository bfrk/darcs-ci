#!/usr/bin/env bash
## Test for issue1879 - we should at least notice that when a patch claims
## to have the same identity (patchinfo) as one of ours, then it should not
## depend on anything we don't have.
##
## Public domain - 2010 Eric Kow

. lib                           # Load some portability helpers.
rm -rf R S                      # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
darcs init      --repo S

cd R
touch x1
darcs add x1
darcs record -am 'x1'
darcs changes --context > ctx
echo hello > f
echo world > x1
darcs add f
darcs record -am 'hello world'
darcs send -a --context ctx -o foo.dpatch ../S
cd ..

cd S
touch x2
darcs add x2
darcs record -am 'x2'
darcs changes --context > ctx
# create an evil wrong patch
sed -e '/Context:/,$d' -e 's/x1/x2/g' ../R/foo.dpatch > foo.dpatch
cat ctx >> foo.dpatch
darcs apply foo.dpatch
cd ..

cd R
# The issue calls for darcs to detect this and fail, which it does,
# though not in a regular way but by calling 'error'. Since the 'not'
# function now regards that as test failure we cannot use it here.
# This is only a temporary work-around: Darcs should never call error
# unless it is really a bug in darcs.
if darcs pull -a ../S 2>&1 | tee log; then
  exit 1
fi
cd ..
