#!/usr/bin/env bash

. ./lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo

# Check that posthook works...
darcs whatsnew -s --posthook 'touch posthook-ran'
test -f posthook-ran
rm posthook-ran

# Check that posthook works with defaults...
echo ALL --posthook touch posthook-ran > _darcs/prefs/defaults
darcs whatsnew -s
test -f posthook-ran
rm posthook-ran

cd ..
rm -rf temp1

# Check that DARCS_PATCHES_XML works
rm -rf R S                      # another script may have left a mess
darcs init      --repo R        # Create our test repos.
darcs init      --repo S        # Create our test repos.

cd R
hook=$(pwd)/hook
cat >$hook.hs <<EOF
import System.Environment
main = getEnv "DARCS_PATCHES_XML" >>= writeFile "hook"
EOF
ghc $hook.hs
darcs record -lam 'hook'
cat > _darcs/prefs/defaults << END
apply run-posthook
apply posthook $(pwd)/hook
END
cd ..

cd S
echo 'Example content.' > f
darcs record -lam 'Add f'
darcs push -a ../R | grep 'patch author'
cd ..
