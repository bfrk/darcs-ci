#!/usr/bin/env bash

# The simplest example of a conflict with dependencies:
# we merge A;B with C, where B depends on A, and C conflicts with A
# and thus also with B. The conflict resolution should not show A
# explicitly as an alternative, but merely show A;B versus C.

. lib

rm -rf base AB C

darcs init base
cd base
touch f
darcs record -lam base
cd ..

darcs clone base AB
cd AB
echo A > f
darcs record -am A
echo B > f
darcs record -am B
cd ..

darcs clone base C
cd C
echo C > f
darcs record -lam C
darcs pull --mark-conflicts -a ../AB
cd ..

cd AB
darcs pull --mark-conflicts -a ../C
cd ..

cat <<EOF > f.expected
v v v v v v v
=============
B
*************
C
^ ^ ^ ^ ^ ^ ^
EOF
diff C/f f.expected >&2
diff AB/f f.expected >&2
