#!/usr/bin/env bash
# This tests that even if broken move patches (caused by issue2674)
# are present in a repo, we can safely eliminate them because no further
# patches can depend on the target of the move. In fact, applying such a
# bad move is a no-op.

. lib

# We could create three different archives for darcs-1, darcs-2, and darcs-3
# but since this problem concerns only the Prim.V1 layer this seems excessive.
only-format darcs-2

# Script to create the test repo. It is no longer used now that issue2674 has
# been fixed and we use the tar ball. Its only remaining purpose is to
# document what patches the broken_move repository consists of.
cat >create_broken_moves.sh <<EOF
  #!/usr/bin/env bash
  . lib

  rm -rf broken_move
  darcs init broken_move
  cd broken_move

  mkdir d1
  darcs record -l d1 -am 'add d1'

  touch f1 f2
  echo text > f1
  darcs move f1 f2 d1
  darcs record -am 'bad first move'

  # this commutes with the two bad moves
  darcs move d1 d2
  darcs record -am 'move d1 to d2'

  darcs move d2/f1 d2/f2 .
  darcs record -am 'bad second move'

  mkdir d3 d4
  darcs move d3 d4 d2
  darcs record -am 'bad third move'

  cd ..

  tar zcf broken_move.tgz broken_move
EOF

chmod +x create_broken_moves.sh

# if create_broken_moves.sh fails, then issue2674 has been fixed,
# so instead we unpack the repo from an archive instead
if ! ./create_broken_moves.sh; then
  unpack_testdata broken_move
fi

cd broken_move

# test that commutes either work or we can repair
echo y | darcs amend -a -p 'move d1 to d2' -m 'edited move d1 to d2'
if ! (echo y | darcs amend -a -p 'bad second move' -m 'edited bad second move' 2> LOG); then
  grep -i "Cannot apply" LOG
  # but then we should be able to repair it
  not darcs check | grep 'Dropping move patch with non-existing source'
  rm -rf ../repaired
  darcs clone . ../repaired
  darcs repair --repodir=../repaired
fi
darcs obliterate -a -p 'move d1 to d2'
if ! (darcs obliterate -a -p 'bad second move' 2>LOG); then
  grep -i "Cannot apply" LOG
  # but then we should be able to repair it
  not darcs check | grep 'Dropping move patch with non-existing source'
  rm -rf ../repaired
  darcs clone . ../repaired
  darcs repair --repodir=../repaired
fi
# test that unapplying patches either works or we can repair the repo
rm -rf ../S
if ! (darcs clone . ../S --to-patch 'add d1' 2>LOG); then
  grep -i "Cannot apply" LOG
  # but then we should be able to repair it
  not darcs check | grep 'Dropping move patch with non-existing source'
  rm -rf ../repaired
  darcs clone . ../repaired
  darcs repair --repodir=../repaired
fi

# test that we cannot record a change that depends on the target path
# nor the source path (because both are unadded)
echo text > f1
not darcs record f1 -am impossible
echo text > d1/f1
not darcs record d1/f1 -am impossible

# same with a bad move of a directory
touch d1/d3/f
# this will ask us to add d1/d3, too, so we have to say no first
echo ny | darcs record -l d1/d3/f -m impossible >LOG
grep "you don't want to record anything" LOG

cd ..

# make a clone to get a fresh working tree equal to the pristine tree
rm -rf R
darcs clone broken_move R
cd R

# check that only d1 exists in pristine, i.e. the bad moves are ignored
cat <<EOF > log.expected
.
./d1
EOF
# the tr hack is to make the test work on Windows
darcs show files | tr -d $'\r' > log
diff log.expected log >&2

cd ..
