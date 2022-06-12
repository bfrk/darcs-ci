#!/usr/bin/env bash

. lib

rm -rf temp1 temp2
mkdir temp1
ln -s temp1 temp2
DIR1="$(pwd)/temp1"
DIR2="$(pwd)/temp2"

cd temp2
darcs init
touch a b c d e f g h i j
darcs add "$DIR1/../temp1/a"
darcs add "$DIR1/../temp2/b"
darcs add "$DIR1/c"
darcs add "$DIR2/../temp1/d"
darcs add "$DIR2/../temp2/e"
darcs add "$DIR2/f"
darcs add "../temp1/g"
darcs add "../temp2/h"
# This should definitely work:
darcs add "i"
# ... as should this one:
mkdir dir
darcs add dir/../j
cd ..
