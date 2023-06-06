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
darcs record -am p62
echo 'u
Q L
P n

n S' > ./a
darcs record -am p67
cd ..

darcs clone B S1
cd S1
echo 'i d
n S' > ./a
darcs record -am p45
cd ..

darcs clone B S2
cd S2
echo '
C
h Z' > ./a
darcs record -am p80
cd ..

darcs clone S1 S
cd S
darcs pull --allow -a ../S2
echo '

ll
G z
h Z' >./a
darcs record -am p35
cd ..

darcs clone B C1
cd C1
darcs pull --allow -a -p p67 ../R
darcs pull --allow -a -p p62 ../R
darcs pull --allow -a -p p45 ../S
darcs pull --allow -a -p p80 ../S
darcs pull --allow -a -p p35 ../S
darcs mark
cd ..

darcs clone B C2
cd C2
darcs pull --allow -a -p p62 ../R
darcs pull --allow -a -p p45 ../S
darcs pull --allow -a -p p67 ../R
darcs pull --allow -a -p p80 ../S
darcs pull --allow -a -p p35 ../S
darcs mark
cd ..

darcs whatsnew --repo C1 > c1
darcs whatsnew --repo C2 > c2

diff c1 c2
