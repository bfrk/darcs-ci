#!/usr/bin/env bash

. ./lib

# test fails for these obsolete formats:
skip-formats darcs-1 darcs-2

rm -rf a ab abc ac b bc acb base

# Set up a base repo. Our experiment will start from this point
mkdir base
cd base
darcs init
printf "Line1\nLine2\nLine3\n" > foo
darcs rec -alm Base
cd ..

# Now we want to record patch A, which will turn "Line2" into "Hello"
darcs get base a
cd a
printf "Line1\nHello\nLine3\n" > foo
darcs rec -am A
cd ..

# Make B the same as A
darcs get base b
cd b
printf "Line1\nHello\nLine3\n" > foo
darcs rec -am B
cd ..

# Now we make a patch C that depends on A
darcs get a ac
cd ac
printf "Line1\nWorld\nLine3\n" > foo
darcs rec -am C
cd ..

# Merge A and B
darcs get a ab
cd ab
darcs pull -a ../b --allow-conflicts
cd ..

# And merge in C too
darcs get ab abc
cd abc
darcs revert -a
darcs pull -a ../ac --allow-conflicts
cd ..

# Now we can pull just B and C into base
darcs get base bc
cd bc
darcs pull ../abc -ap 'B|C' --allow-conflicts
cd ..

# Now we have base, B and C in a repository.  At this point we're correct.

# Let's try merging AC with BC now, here we discover a bug.

darcs get ac acb
cd acb
darcs pull -a ../bc
darcs changes
test `darcs changes | fgrep -c '* C'` -eq 1
cd ..
