#!/usr/bin/env bash
# test for issue2682: conflict not marked if tag pulled at the same time

. lib

rm -rf R S
darcs init R
cd R
echo initial>file
darcs add file
darcs record -am initial
darcs clone . ../S

# Record a change in R
echo one > file
darcs record -am one
cd ../S

# Record a conflicting change in S
echo two > file
darcs record -am two
cd ..

tag() {
  name=T$1
  rm -rf $name
  darcs clone $1 $name
  cd $name
  darcs tag $name
  cd ..
}

depend() {
  name=D$1
  rm -rf $name
  darcs clone $1 $name
  cd $name
  echo yd | darcs record --ask-deps -m $name
  cd ..
}

runtest() {
  cd $2
  # Pull from R to S
  # Darcs should say there's a conflict and
  # mark it, but instead the pull silently succeeds.
  darcs pull ../$1 -a 2>&1 | tee LOG
  grep -i conflicts LOG
  darcs whatsnew

  # undo the pull
  darcs revert -a
  echo y | darcs obliterate -a --last=2
  # again, this time with --reorder-patches
  darcs pull ../$1 -a --reorder-patches 2>&1 | tee LOG
  grep -i conflicts LOG
  darcs whatsnew
  cd ..
}

# test all 4 combinations of tag/depend
tag R
depend R

tag S
runtest TR TS
depend S
runtest TR DS
tag S
runtest DR TS
depend S
runtest DR DS
