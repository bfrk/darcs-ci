#!/usr/bin/env bash
. ./lib

rm -rf temp
mkdir temp
cd temp

darcs init

echo "X X X" > foo
echo $'A A,A\tA,A\vA' >> foo
echo "aä" >> foo
echo $'\02' >> foo
darcs rec -alm "Added"

# These should fail until replace handles tokens and
# token-chars with leteral spaces in them
not darcs replace ' X ' ' XX ' --token-chars '[ X]' foo
not darcs replace $'A A'  'aaa' --token-chars '[^,]' foo
not darcs replace $'A\tA' 'aaa' --token-chars '[^,]' foo
not darcs replace $'A\vA' 'aaa' --token-chars '[^,]' foo

# These should fail since darcs cannot handle non-ASCII token chars
# nor non-printable ones
not darcs replace 'X' 'ä' --token-chars '[Xä]' foo
not darcs replace 'ä' 'X' --token-chars '[ä]' foo
not darcs replace $'\02' 'X' --token-chars $'[X\02]' foo
not darcs replace 'X' $'\02' --token-chars $'[X\02]' foo

# Check that replace is not fooled by duplicate file names
# (i.e. not trying to performe the replace twice in the same file)
darcs replace X Y foo foo
darcs replace Y Z foo ../temp/foo
darcs replace Z Q foo foo --repodir=../temp/
darcs rec -am "xyzq"


# Try to "overwrite" a hunk with a replace.
#
# v1.0.8 accepts this without error or warning,
# but should perhaps require the --force option?
#
# current unstable sometimes(!) fails with bug: invalid pending
# which is surely a bug.

# this succeeds
echo "x" > foo
darcs rec -am xx
echo "y" > foo
darcs replace x y foo

# this fails
echo "hej" > foo
darcs rec -am hej
echo "hopp" > foo
darcs replace hej hopp foo

darcs whatsnew

echo "src" > foo
echo "dst" >> foo
darcs rec -am hop
darcs replace src dst foo || true
darcs replace --force src dst foo

darcs whatsnew
darcs whatsnew -ls

cd ..
rm -rf temp

# replace after pending add

mkdir temp1
cd temp1
darcs init

echo a b a b a b > A
darcs add A
darcs replace a c A > LOG
not grep Skipping LOG
cd ..

rm -fr temp1

# replace after pending mv

mkdir temp1
cd temp1
darcs init

echo a b a b a b > A
darcs add A
darcs record --all --name=p1
darcs mv A B
darcs replace a c B > LOG
not grep Skipping LOG
cd ..

rm -fr temp1

