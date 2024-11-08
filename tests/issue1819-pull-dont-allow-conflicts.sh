#!/usr/bin/env bash
## Test for issue1819 - pull --dont-allow-conflicts doesn't work
##
## Dave Love <fx@gnu.org>, Public domain

. lib
rm -rf R S
for repo in R S; do
    darcs init --repo $repo
    cd $repo
    echo 'Example content.' >x
    darcs add x
    darcs record -lam 'Add x'
    echo $repo >x
    darcs record -lam 'Change x'
    cd ..
done

rm -rf S0
darcs get S S0
cd S0
darcs pull --no-pause-for-gui --all --external-merge 'cp %2 %o' ../R
cd ..

rm -rf S0b
darcs get S S0b
cd S0b
not darcs pull --no-pause-for-gui --all --dont-allow-conflicts ../R
cd ..

# --external-merge is now in the same set of mutually exclusive options
# as the --{[no-]allow,mark}-conflicts, so passing two of them
# should result in an error.

rm -rf S1
darcs get S S1
cd S1
not darcs pull --no-pause-for-gui --all --external-merge 'cp %2 %o' --dont-allow-conflicts ../R 2>log
grep -i 'conflicting options' log
cd ..

rm -rf S2
darcs get S S2
cd S2
not darcs pull --no-pause-for-gui --all --dont-allow-conflicts --external-merge 'cp %2 %o' ../R 2>log
grep -i 'conflicting options' log
cd ..
