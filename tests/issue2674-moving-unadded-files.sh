. lib

rm -rf R
darcs init R
cd R
echo added >added.txt
echo unadded >unadded.txt
mkdir newdir
darcs add added.txt newdir/
not darcs mv added.txt unadded.txt newdir/ > LOG
not grep unadded LOG
# i.e. not this:
#Finished moving: ./added.txt ./unadded.txt to: ./newdir
darcs whatsnew -s > LOG
not grep unadded LOG
# i.e. not this:
# A ./newdir/
#  ./unadded.txt -> ./newdir/unadded.txt
# A ./newdir/added.txt
cd ..
