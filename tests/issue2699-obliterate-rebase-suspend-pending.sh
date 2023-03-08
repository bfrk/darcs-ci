# test for issue2699

. lib
rm -rf R S
darcs init R
cd R

mkdir d
darcs record -lam add-d
darcs move d e
darcs record -am move-d-e

darcs clone . ../S

touch e/f
darcs add e/f
echo yy | darcs obliterate -p move-d-e
cat >../pending_expected <<EOF
addfile ./d/f
EOF
diff _darcs/patches/pending ../pending_expected >&2

cd ../S

touch e/f
darcs add e/f
echo yy | darcs rebase suspend -p move-d-e
diff _darcs/patches/pending ../pending_expected >&2

cd ..
