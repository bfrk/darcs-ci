#!/usr/bin/env bash

# Note that this documents the current behavior; I (bf) don't claim it makes
# any sense. My personal opinion is that using absolute paths as arguments to
# denote files tracked by a repository is dubious at best and I don't think we
# should promise that it works at all. Similarly with relative paths that
# leave (so to speak) the current working tree via '..' components, even if
# afterwards they "return" to it.

. lib

rm -rf temp1 temp2
mkdir temp1
ln -s temp1 temp2
# Our hspwd resolves temp2 to temp1 on Linux and MacOS, but not on Windows.
# To make conditions equal, we explicitly tell which directory name to use.
DIR1="$(pwd)/temp1"
DIR2="$(pwd)/temp2"

cd temp2
darcs init
touch a b c d e f g h i j
darcs add "$DIR1/../temp1/a"
not darcs add "$DIR1/../temp2/b"
darcs add "$DIR1/c"
darcs add "$DIR2/../temp1/d"
not darcs add "$DIR2/../temp2/e"
not darcs add "$DIR2/f"
darcs add "../temp1/g"
not darcs add "../temp2/h"
# This should definitely work:
darcs add "i"
# ... as should this one:
mkdir dir
darcs add dir/../j
cd ..
