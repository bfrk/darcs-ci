#!/usr/bin/env bash

# This tests that a minor change to the format of inventory files introduced
# in darcs-2.17.2 is compatible with previous releases. The situation is when
# a tag is recorded in an empty repository. This now creates and refers to an
# empty inventory, whereas previously it did not.

. lib

# forward compatibility

rm -rf empty-old empty-new
darcs init empty-new
cd empty-new
darcs tag XX
# we have not (semantically) changed the functions that read
# inventories so this suffices for testing forward compatibility
test $(darcs log --count) = "1"
cd ..

# backward compatibility

unpack_testdata empty-old
cd empty-old
# read
test $(darcs log --count) = "1"
# write
darcs tag YY
echo y | darcs obliterate -a
cd ..
