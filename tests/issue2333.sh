#!/usr/bin/env bash
## Test for issue2333 - <SYNOPSIS: Error message when pushing and darcs not in path.>

. lib                           # Load some portability helpers.

require_ghc 706

thedarcs=$(type -P darcs)
# on macOS with ghc >= 9.6 running $thedarcs with an empty PATH fails with
# darcs: security: createProcess: posix_spawnp: does not exist (No such file or directory)
#(echo $thedarcs | grep -q osx) && exit 200

darcs init      --repo R        # Create our test repos.
darcs init      --repo S

cd R
echo 'Example content.' > f
darcs record -lam 'Add f.'
PATH='' $thedarcs push ../S -a --debug >&2 # Try to push patches between repos.
cd ..
