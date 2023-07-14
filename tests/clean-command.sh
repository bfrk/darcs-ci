#!/usr/bin/env bash
# Tests for the clean / revert -l command

. lib

rm -rf R
darcs init R
cd R

create_stuff () {
  rm -rf unadded unadded-dir unadded-dir-with-boring boring.o CVS

  # non-boring stuff
  echo content > unadded
  mkdir unadded-dir
  echo content > unadded-dir/unadded
  mkdir unadded-dir-with-boring
  echo content > unadded-dir-with-boring/unadded

  # boring stuff
  echo content > boring.o
  mkdir CVS
  echo content > CVS/also-considered-boring
  echo content > CVS/boring.o
  echo content > unadded-dir-with-boring/boring.o
}

test_nonboring () {
  # test that non-boring stuff is gone
  not ls unadded
  not ls unadded-dir
  not ls unadded-dir-with-boring/unadded
  # non-boring file under boring dir is still considered non-boring
  not ls CVS/unadded

  # test that boring stuff is unchanged
  diff unadded-dir-with-boring/boring.o <(echo content)
  diff boring.o <(echo content)
  diff CVS/boring.o <(echo content)
}

test_boring () {
  not ls unadded
  not ls unadded-dir
  not ls unadded-dir-with-boring
  not ls boring.o
  not ls CVS
}

create_stuff
darcs clean -a
test_nonboring

create_stuff
darcs revert -l -a
test_nonboring

create_stuff
darcs clean --boring -a
test_boring

create_stuff
# error: conflicting options
not darcs revert -l --boring -a

cd ..
