#!/usr/bin/env bash
# rebase with conflicting unrecorded changes
# TODO repeat test for rebase pull and rebase apply

. lib

rm -rf R
darcs init R
cd R
echo orig > file
darcs record -lam thepatch
darcs replace -f changed orig file
echo changed > file
darcs whatsnew > ../whatsnew.orig
# make sure we have a change in the pending patch as well
grep replace ../whatsnew.orig

# non-interactive mode: should fail
not darcs rebase suspend -p thepatch -a 2>ERR
grep "Can't suspend .* without reverting" ERR
darcs whatsnew > ../whatsnew.failed
diff ../whatsnew.orig ../whatsnew.failed

# interactive mode: user gets prompted
# first y to select patch, second y to confirm selection, n to refuse revert
echo yyn | darcs rebase suspend -p thepatch
darcs whatsnew > ../whatsnew.refuse
diff ../whatsnew.orig ../whatsnew.refuse
# first y to select patch, second y to confirm selection, third y to accept revert
echo yyy | darcs rebase suspend -p thepatch
# all unrecorded changes are reverted now
not darcs whatsnew
