. ./lib

# the epoch we use below is Unix specific
abort_windows

rm -rf R
darcs init R
cd R

touch b
darcs record -lam 'add b'

# create a file with zero size and timestamp
touch -d "1970-01-01T00:00:00 UTC" f
darcs add f
rm _darcs/index
# this should not crash darcs:
darcs whatsnew -s
