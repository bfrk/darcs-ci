#!/usr/bin/env bash

. lib

rm -rf jhc lhc

# Set up repository to represent JHC's situation -- all sources in the root
mkdir jhc
cd jhc
darcs init
echo content > Foo
darcs rec -alm Base
cd ..

# Now create another one to represent LHC -- sources in src/ dir
darcs get jhc lhc
cd lhc
mkdir src/
darcs add src/
darcs mv Foo src/Foo
darcs rec -am "Move sources into src/"
# ... and change something
echo content1 > src/Foo
darcs rec -am "content1"
cd ..

# change something different in JHC
cd jhc
echo content2 > Foo
darcs rec -am "content2"
cd ..

# our external merge tool checks that the arguments
# exist and have the expected content
cat > external_merge.hs <<EOF
import Control.Monad
import System.Environment
import System.Exit
main = do
  [ca,c1,c2,co] <- getArgs >>= mapM readFile
  when (ca /= "content\n") exitFailure
  when (c1 /= "content1\n") exitFailure
  when (c2 /= "content2\n") exitFailure
  when (co /= "content\n") exitFailure
EOF
ghc --make external_merge.hs
merge_tool=$(pwd)/external_merge

# try to merge them with our external_merge script
cd lhc
darcs pull -a ../jhc --no-pause-for-gui --external-merge="$merge_tool %a %1 %2 %o"
cd ..
