#!/usr/bin/env bash

. lib

case $format in
  darcs-1)
    v=v1
    ;;
  darcs-2)
    v=v2
    ;;
  darcs-3)
    exit # no legacy formats!
    ;;
esac

rm -rf empty
mkdir empty
cd empty
darcs init
cd ..

unpack_testdata context-$v

rm -rf R
darcs clone empty R
cd R
darcs apply $TESTDATA/legacy/context-$v.dpatch
darcs pull ../repo | grep 'No remote patches to pull'
darcs push ../repo | grep 'No recorded local patches to push'
cd ..

if test "$v" = "v2"; then
  repos="resolution simple threewayanddep threewayandmultideps threewayconflict tworesolutions twowayconflict"
  for r in $repos ; do
    rm -rf R
    darcs clone empty R
    cd R
    darcs apply --allow-conflicts $TESTDATA/legacy/darcs2/$r.dpatch >&2
    cd ..
  done
fi
