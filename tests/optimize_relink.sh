
#!/usr/bin/env bash

# For issue600, testing optimize --relink 

. ./lib

## compare succeeds if all files in $1 are also found under $2 and are hard links
compare () {
  ls -1 $1 | grep -v pend | while read fn; do
  test -d $2
    fn1=$1/$fn
    fn2=$(find $2 -name $fn)
    # only if the other file actually exists
    if test -n "$fn2"; then
      test "$(stat -c %i $fn1)" = "$(stat -c %i $fn2)"
    fi
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
# so that we have something in inventories
darcs tag sometag
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

# copy the repo
cp -r x y

hashed_dirs="patches inventories pristine.hashed"

if grep no-cache $HOME/.darcs/defaults; then
  testing_with_cache="no"
else
  testing_with_cache="yes"
fi

check () {
  for d in $hashed_dirs; do
    compare x/_darcs/$d y/_darcs/$d
    if $testing_with_cache; then
      compare x/_darcs/$d $HOME/.cache/darcs/$d
    fi
  done
}

## Now try relinking using darcs.

# first: sanity check
for d in $hashed_dirs; do
  ! compare x/_darcs/$d y/_darcs/$d
  if test "$testing_with_cache" = "yes"; then
    ! test -d $HOME/.cache/darcs/$d
  fi
done

# now do it
darcs optimize relink --verbose --repodir x --sibling y --debug 2> ERR1

# test that it did what it should
for d in $hashed_dirs; do
  compare x/_darcs/$d y/_darcs/$d
  if test "$testing_with_cache" = "yes"; then
    compare x/_darcs/$d $HOME/.cache/darcs/$d
  fi
done

## test that optimize relink does not destroy sharing

rm -rf y z
darcs clone x y
darcs clone y z
# this is needed because when we clone a repo, the pristine hash
# for the empty working tree is created before we initialize sources
# and defaultrepo, and thus isn't shared:
darcs optimize clean --repo z
# hashed files should be shared between x, y, z, and global cache
for d in $hashed_dirs; do
  compare x/_darcs/$d y/_darcs/$d
  compare y/_darcs/$d z/_darcs/$d
  if test "$testing_with_cache" = "yes"; then
    compare x/_darcs/$d $HOME/.cache/darcs/$d
  fi
done
# if cache is enabled, we can even remove any implicit siblings
if test "$testing_with_cache" = "yes"; then
  rm z/_darcs/prefs/sources
  rm z/_darcs/prefs/defaultrepo
fi
# do not pass any explicit siblings
darcs optimize relink --verbose --repodir z --debug 2> ERR2
for d in $hashed_dirs; do
  compare x/_darcs/$d y/_darcs/$d
  compare y/_darcs/$d z/_darcs/$d
  if test "$testing_with_cache" = "yes"; then
    compare x/_darcs/$d $HOME/.cache/darcs/$d
  fi
done

cd ..
