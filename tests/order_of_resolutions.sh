#!/usr/bin/env bash

. lib

rm -rf B R S S1 S2 C1 C2

darcs init B
cd B
echo '

n S' > ./a
darcs record -l ./a -am base
cd ..

darcs clone B R
cd R
darcs replace K u ./a
darcs record -am p1
echo 'u
Q L
P n

n S' > ./a
darcs record -am p2
cd ..

darcs clone B S1
cd S1
echo 'i d
n S' > ./a
darcs record -am p3
cd ..

darcs clone B S2
cd S2
echo '
C
h Z' > ./a
darcs record -am p4
cd ..

darcs clone S1 S
cd S
darcs pull --allow -a ../S2
echo '

ll
G z
h Z' >./a
darcs record -am p5
cd ..

darcs clone B C1
cd C1
darcs pull --allow -a -p p2 ../R
darcs pull --allow -a -p p1 ../R
darcs pull --allow -a -p p3 ../S
darcs pull --allow -a -p p4 ../S
darcs pull --allow -a -p p5 ../S
darcs mark
cd ..

darcs clone B C2
cd C2
darcs pull --allow -a -p p1 ../R
darcs pull --allow -a -p p3 ../S
darcs pull --allow -a -p p2 ../R
darcs pull --allow -a -p p4 ../S
darcs pull --allow -a -p p5 ../S
darcs mark
cd ..

darcs whatsnew --repo C1 > markup-C1
darcs whatsnew --repo C2 > markup-C2

diff markup-C1 markup-C2
