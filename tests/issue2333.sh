#!/usr/bin/env bash
## Test for issue2333 - <SYNOPSIS: Error message when pushing and darcs not in path.>

. lib                           # Load some portability helpers.

require_ghc 706

darcs init      --repo R        # Create our test repos.
darcs init      --repo S

cd R
echo 'Example content.' > f
darcs record -lam 'Add f.' --debug
thedarcs=$(type -P darcs)
env >&2
PATH='' $thedarcs push ../S -a	--debug        # Try to push patches between repos.
cd ..
