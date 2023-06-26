. lib

# note: we enforce --no-cache otherwise this is hard to reproduce

rm -rf R S
darcs init R
cd R
echo x > x
darcs record --no-cache x -lam add_x
rm -f _darcs/pristine.hashed/*
cd ..
not darcs clone  --no-cache R S 2>&1 | grep repair
