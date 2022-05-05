. lib

rm -rf R
darcs init R
cd R

touch x1 y1
darcs rec -lam "adds"

darcs mv x1 x2
darcs mv y1 y2

rm x2 y2
darcs whatsnew > out
cat out >&2
not grep move out

cd ..
