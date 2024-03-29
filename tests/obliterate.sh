#!/usr/bin/env bash
. ./lib

# Part 1

rm -rf temp1
darcs initialize temp1
cd temp1

echo hello world > foo
darcs record -l -a -m hellofoo
echo goodbye world >> foo
darcs record -a -m goodbyefoo
darcs replace world bar foo
echo Hi there foo > bar
darcs record -l -a -m addbar
darcs mv bar baz
darcs replace bar baz foo
darcs record -a -m bar2baz
echo Do not love the baz, or anything in the baz. >> foo
darcs record -a -m nolove
darcs mv baz world
darcs replace baz world foo
darcs record -a -m baz2world

not darcs whatsnew

grep 'love the world' foo

echo yy | darcs obliterate -p baz2world

not darcs whatsnew

grep 'love the baz' foo

echo yy | darcs obliterate -p bar2baz

grep 'love the bar' foo

echo yy | darcs obliterate -p nolove

grep 'love' foo && exit 1 || true

cd ..
rm -rf temp1

# Part 2

darcs init temp1
cd temp1

touch a.txt
darcs add a.txt
darcs record -a -m 'adding a' a.txt
touch b.txt
darcs add b.txt
darcs record -a -m 'adding b' b.txt
# extra confirmation for --all
echo an | darcs obliterate -p add | grep -i "really obliterate"
# --last=1
echo nyy | darcs obliterate --last 1 | grep -i adding
# automatically getting dependencies
date >> a.txt
darcs record -a -m 'modifying a' a.txt
echo ny | darcs obliterate -p 'adding a' > log
grep -i "modifying a" log
grep -i "No patches selected" log
cd ..
rm -rf temp1

# Part 3

darcs init temp1
cd temp1

echo foo > foo
darcs record -l -a -m 'addfoo'

darcs obliterate -a

not darcs whatsnew

cd ..
rm -rf temp1

# Part 4

darcs init temp1
cd temp1

cat > f <<EOF
one
two
three
EOF
darcs rec -Ax -alm init
cp f g
cat > f <<EOF
three
one
EOF
darcs rec -Ax -am foo
echo yy | darcs unpull -p foo
cmp f g
cd ..
rm -rf temp1

# Part 4: obliterate with conflicting unrecorded changes

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
not darcs obliterate -p thepatch -a 2>ERR
grep "Can't obliterate .* without reverting" ERR
darcs whatsnew > ../whatsnew.failed
diff ../whatsnew.orig ../whatsnew.failed

# interactive mode: user gets prompted
# first y to select patch, second y to confirm selection, n to refuse revert
echo yyn | darcs obliterate -p thepatch
darcs whatsnew > ../whatsnew.refuse
diff ../whatsnew.orig ../whatsnew.refuse
# first y to select patch, second y to confirm selection, third y to accept revert
echo yyy | darcs obliterate -p thepatch
# all unrecorded changes are reverted now
not darcs whatsnew
