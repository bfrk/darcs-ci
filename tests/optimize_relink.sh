
#!/usr/bin/env bash

# For issue600, testing optimize --relink 

. ./lib

## compare succeeds if all files in $1 are also in $2 and are hard links
compare () {
  ls -1 $1 | while read fn; do
    test "$(stat -c '%W %X %Y %Z' $1/$fn)" = "$(stat -c '%W %X %Y %Z' $2/$fn)"
  done
}

rm -rf temp
mkdir temp
cd temp

mkdir x
darcs init --repodir x
cd x
date > foo
darcs add foo
darcs record -a -A me -m 'addfoo'
cd ..

## Does the filesystem support hard linking at all?
mkdir z1
echo "hi" > z1/foo
mkdir z2
if ! ln z1/foo z2/foo ; then
  echo No ln command for `pwd` - assuming no hard links.
  exit 200
fi
if ! compare z1 z2 ; then
  echo Filesystem for `pwd` does not support hard links.
  exit 200
fi
cp -r x y

## Now try relinking using darcs.
rm -rf z
darcs optimize relink --verbose --repodir x --sibling y
rm -rf x/_darcs/patches/pend* y/_darcs/patches/pend*
compare x/_darcs/patches y/_darcs/patches

cd ..
