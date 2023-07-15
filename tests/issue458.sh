#!/usr/bin/env bash
### http://bugs.darcs.net/issue458
### darcs get --set-scripts-executable ignores umask
. ./lib

# We can set and clear permission bits with bash on Windows but that
# has not the expected effect on programs. So even though this test
# actually succeeds on Windows, it makes no sense to run it.
abort_windows

rm -rf temp
mkdir temp
cd temp

mkdir repo1
darcs initialize --repodir repo1
printf >repo1/x '#!/bin/sh\ntrue'   # make a shebang'd script
darcs record --repodir repo1 -lam x x
umask 077                       # DENY ALL access to group, all
darcs get --set-scripts-executable repo1 repo2
# remove trailing-dot for xattr
ls -l repo2/x | cut -f 1 -d\  | sed -e "s/\.$//" > mode
echo -rwx------ > desired-mode
diff -u desired-mode mode

cd ..
