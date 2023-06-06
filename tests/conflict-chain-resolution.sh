#!/usr/bin/env bash

# Test of conflict resolution for a "chain" of conflicts
# i.e. where (only) the adjacent patches conflict.

. lib

# Note that this test fails for darcs-2 format.
skip-formats darcs-2

# Establish our baseline:

rm -rf C
darcs init C
cd C
cat <<EOF > f
a
b
c
d
e
f
EOF
darcs record -lam "base"
cd ..

# Record hunk patches p1..p5 in separate repos,
# such that (only) adjacent patches conflict

rm -rf R1
darcs clone -q C R1
cd R1
sed -i -e 's/[ab]/&1/' f
darcs record -am 'p1'
cd ..

rm -rf R2
darcs clone -q C R2
cd R2
sed -i -e 's/[bc]/&2X/' f
darcs record -am 'p2X'
sed -i -e 's/2X/2/' f
darcs record -am 'p2'
cd ..

rm -rf R3
darcs clone -q C R3
cd R3
sed -i -e 's/[cd]/&3/' f
darcs record -am 'p3'
cd ..

rm -rf R4
darcs clone -q C R4
cd R4
sed -i -e 's/[de]/&4/' f
darcs record -am 'p4'
cd ..

rm -rf R5
darcs clone -q C R5
cd R5
sed -i -e 's/[ef]/&5/' f
darcs record -am 'p5'
cd ..

# Pull them all into the common context

cd C
darcs pull -a ../R*
cd ..

baseline='
a
b
c
d
e
f
'

# p1 .. p5 are for darcs-3

p1='
a1
b1
c
d
e
f
'

p2='
a
b2
c2
d
e
f
'

p2X='
a
b2X
c2X
d
e
f
'

p3='
a
b
c3
d3
e
f
'

p4='
a
b
c
d4
e4
f
'

p5='
a
b
c
d
e5
f5
'

# p25 .. p135 are for darcs-1

# The maximal non-conflicting subsets are:
#  {p1, p3, p5}, {p1, p4}, {p2, p4} {p2, p5}
# They are what I would expect conflict resolution
# to display as alternatives to the baseline.

p25='
a
b2
c2
d
e5
f5
'

p24='
a
b2
c2
d4
e4
f
'

p14='
a1
b1
c
d4
e4
f
'

p135='
a1
b1
c3
d3
e5
f5
'

cd C
# now check that:

# * there is exactly one conflict resolution
test $(grep -cF 'v v v v v v v' f) = 1
test $(grep -cF '=============' f) = 1
test $(grep -cF '^ ^ ^ ^ ^ ^ ^' f) = 1

cat f | tr '\n' 'X' > xf

declare -a alternatives
if test "$format" = "darcs-3"; then
  alternatives=("$baseline" "$p1" "$p2" "$p2X" "$p3" "$p4" "$p5")
else
  alternatives=("$baseline" "$p25" "$p24" "$p14" "$p135")
fi

# * each of the alternatives occur at least once
cat f | tr '\n' 'X' > xf
for p in $alternatives; do
  grep $(echo -n "$p" | tr '\n' 'X') xf
done

# * there are (number of alternatives to the baseline minus one) separators
test $(grep -cF '*************' f) = $(($#alternatives - 1))

cd ..
