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

# coalescing should result in 2 changes plus 1 last regrets prompt
echo yyy | darcs record -m "rms"
darcs log -v --last=1 > log
not grep move log

cd ..

# same situation but record only one of the coalesced changes

rm -rf R
darcs init R
cd R

touch x1 y1
darcs rec -lam "adds"

darcs mv x1 x2
rm x2

darcs whatsnew > whx

darcs mv y1 y2
rm y2

# record only the 'rmfile ./y1'
echo nyy | darcs record -m "rms"
darcs log -v --last=1 > log
not grep move log
grep -F 'rmfile ./y1' log
grep move _darcs/patches/pending
darcs whatsnew > whx_after
diff -u whx whx_after >&2

cd ..
