#!/usr/bin/env bash

. lib

rm -rf R
darcs init R
cd R

echo 'version1' > file
darcs record -lam "version1"

# force replace so we have a hunk in pending that we can split
# but no (additional) changes detected in the working tree
darcs replace -f version2 version1 file

echo eyd | DARCS_EDITOR="sed -i -e s/version2/version1.5/" darcs record -m "version1.5" >&2

# test that we correctly removed the recorded hunk which we split off from
# the forced hunk in pending:

cat > expected <<EOF
hunk ./file 1
-version1.5
+version2
replace ./file [A-Za-z_0-9] version2 version1
EOF

darcs whatsnew | diff -u expected - >&2

cd ..

# same test as above but now *with* additional changes in working

rm -rf R
darcs init R
cd R

echo 'version1' > file
touch file2
darcs record -lam "version1"

# force replace so we have a hunk in pending that we can split
darcs replace -f version2 version1 file
darcs move file2 file3
echo text > file3

echo neyd | DARCS_EDITOR="sed -i -e s/version2/version1.5/" darcs record -m "version1.5" >&2

# test that we correctly removed the recorded hunk which we split off from
# the forced hunk in pending:

cat > expected <<EOF
move ./file2 ./file3
hunk ./file 1
-version1.5
+version2
replace ./file [A-Za-z_0-9] version2 version1
hunk ./file3 1
+text
EOF

darcs whatsnew | diff -u expected - >&2

cd ..
