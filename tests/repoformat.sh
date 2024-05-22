#!/usr/bin/env bash

. lib

rm -rf garbage
mkdir garbage
cd garbage
darcs init
echo gobbledygook >> _darcs/format
cd ..

rm -rf future
mkdir future
cd future
darcs init
touch titi
darcs add titi
darcs record -am titi
sed -i 's/hashed/hashed|gobbledygook/' _darcs/format
cd ..

corrupt_format_file() {
  # mimic format file corruption as in issue2650
  sed -i 's/gobbledygook/Unknown format: gobbledygook/' $1/_darcs/format
}

# check the rules for reading and writing

test_garbage() {
## garbage repo: we don't understand anything
rm -rf temp1
not darcs get garbage temp1 2> log
grep -i "read repository.*unknown format" log

# pull from garbage repo
rm -rf temp1
mkdir temp1
cd temp1
darcs init
not darcs pull ../garbage 2> log
grep -i "read repository.*unknown format" log
cd ..

# apply in garbage repo
rm -rf temp1
mkdir temp1
cd temp1
darcs init
darcs changes --context > empty-context
darcs tag -m "just a patch"
darcs send -a --context=empty-context -o ../bundle.dpatch .
cd ../garbage
not darcs apply ../bundle.dpatch 2> log
grep -i "read repository.*unknown format" log
cd ..

# add in garbage repo
cd garbage
touch toto

cp _darcs/format before
not darcs add toto 2> log
grep -i "read repository.*unknown format" log
diff before _darcs/format # issue2650
cd ..

# rebase suspend in garbage repo
cd garbage
cp _darcs/format before
not darcs rebase suspend --last=1 2> log
grep -i "read repository.*unknown format" log
diff before _darcs/format # issue2650
cd ..
}

test_garbage

# corrupt once
corrupt_format_file garbage
test_garbage
# corrupt multiple times
corrupt_format_file garbage
corrupt_format_file garbage
test_garbage


## future repo: we don't understand one
#  alternative of a line of format

test_future () {
# get future repo: ok
# --to-match is needed because of bug###
rm -rf temp1
darcs get future temp1 --to-match "name titi"
# fresh clone should fix corrupt format
cd temp1
not grep 'Unknown format' _darcs/format # issue2650
darcs changes
not grep 'Unknown format' _darcs/format # issue2650
touch toto
darcs add toto
not grep 'Unknown format' _darcs/format # issue2650
darcs record -am 'blah'
not grep 'Unknown format' _darcs/format # issue2650
cd ..

# pull from future repo: ok
rm -rf temp1
mkdir temp1
cd temp1
darcs init
darcs pull ../future -a
darcs changes | grep titi
not grep 'Unknown format' _darcs/format # issue2650
cd ..

# apply in future repo: !ok
rm -rf temp1
mkdir temp1
cd temp1
darcs init
darcs changes --context > empty-context
darcs tag -m "just a patch"
darcs send -a --context=empty-context -o ../bundle.dpatch .
cd ../future
cp _darcs/format before
not darcs apply ../bundle.dpatch 2> log
cat log
grep -i "write repository.*unknown format" log
diff before _darcs/format # issue2650
cd ..

# record in future repo: !ok
cd future
touch toto
not darcs add toto 2> log
grep -i "write repository.*unknown format" log
cd ..

# rebase suspend in future repo
cd future
cp _darcs/format before
not darcs rebase suspend --last=1 2> log
grep -i "write repository.*unknown format" log
diff before _darcs/format # issue2650
cd ..
}

test_future

# corrupt once
corrupt_format_file future
test_future
# corrupt multiple times
corrupt_format_file future
corrupt_format_file future
test_future
