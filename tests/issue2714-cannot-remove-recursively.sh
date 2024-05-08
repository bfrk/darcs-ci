. lib

darcs init R
cd R
mkdir -p src/System/Foo/Bar
darcs record -lam "add directories"
# darcs show files >&2
darcs remove --recursive src/
cd ..
