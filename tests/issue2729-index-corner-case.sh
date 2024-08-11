. ./lib

# the epoch we use below is Unix specific
abort_windows

rm -rf R
darcs init R
cd R

# create a file with zero size and timestamp
touch -d "1970-01-01 00:00:00 UTC" f
darcs record -lam 'add f'
# this should not crash darcs:
darcs unrecord -a
