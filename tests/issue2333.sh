#!/usr/bin/env bash
## Test for issue2333 - <SYNOPSIS: Error message when pushing and darcs not in path.>

. lib                           # Load some portability helpers.

# work around issue2720 (MacOS)
if test -x /usr/bin/security; then
  ln -s /usr/bin/security .
fi

darcs init      --repo R        # Create our test repos.
darcs init      --repo S

cd R
echo 'Example content.' > f
darcs record -lam 'Add f.'
thedarcs=$(type -P darcs)
PATH='..' $thedarcs push ../S -a	        # Try to push patches between repos.
cd ..
