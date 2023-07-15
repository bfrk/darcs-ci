
#!/usr/bin/env bash

# For issue600, testing optimize --relink 

. ./lib

## compare succeeds if all files in $1 are also found under $2 and are hard links
compare () {
  ls -1 $1 | while read fn; do
    test -d $2
    fn2=$(find $2 -name $fn)
    test "$(stat -c %i $1/$fn)" = "$(stat -c %i $fn2)"
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
# use --no-cache so that the cache does not (yet) have a copy/link
darcs record -a -A me -m 'addfoo' --no-cache
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

hashed_dirs="patches inventories pristine.hashed"

if grep no-cache $HOME/.darcs/defaults; then
  testing_with_cache="no"
else
  testing_with_cache="yes"
fi

if os_is_windows; then
  cachedir=$DARCS_TESTING_PREFS_DIR/cache2
else
  cachedir=$HOME/.cache/darcs
fi

check () {
  for d in $hashed_dirs; do
    compare x/_darcs/$d y/_darcs/$d
    if $testing_with_cache; then
      compare x/_darcs/$d $cachedir/$d
    fi
  done
}

## Now try relinking using darcs.

# first: sanity check
for d in $hashed_dirs; do
  ! compare x/_darcs/$d y/_darcs/$d
  if test "$testing_with_cache" = "yes"; then
    ! test -d $cachedir/$d
  fi
done

# now do it
darcs optimize relink --verbose --repodir x --sibling y --debug

# get unhashed files out of the way
rm -rf x/_darcs/patches/pend*

# test that it did what it should
for d in $hashed_dirs; do
  compare x/_darcs/$d y/_darcs/$d
  if test "$testing_with_cache" = "yes"; then
    compare x/_darcs/$d $cachedir/$d
  fi
done

cd ..
