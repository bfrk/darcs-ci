#!/bin/sh -e
##
## Tests with untestable states
##
## Copyright (C) 2015 Ganesh Sittampalam
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib

cat > runtest.hs <<END
import Control.Monad
import System.Exit
import System.IO
import System.Directory
main = do
  exists <- doesFileExist "teststatus"
  when exists $ do
    n <- liftM read (readFile "teststatus")
    when (n /= 0) (exitWith (ExitFailure n))
END

ghc --make -o runtest runtest.hs

export RUNTEST=`pwd`/runtest

mkdir R
cd R
darcs init

echo 0 > teststatus
darcs add teststatus
darcs rec -am "teststatus=0"

echo 125 > teststatus
darcs rec -am "teststatus=125"

echo 1 > teststatus
darcs rec -am "teststatus=1"

darcs test --linear "$RUNTEST" > results

grep -A10 "These patches jointly trigger the failure" results > patchlist

grep "teststatus=1" patchlist
grep "teststatus=125" patchlist
not grep "teststatus=0" patchlist

darcs test --backoff "$RUNTEST" > results

grep -A10 "These patches jointly trigger the failure" results > patchlist

grep "teststatus=1" patchlist
grep "teststatus=125" patchlist
not grep "teststatus=0" patchlist

darcs test --bisect "$RUNTEST" > results

grep -A10 "These patches jointly trigger the failure" results > patchlist

grep "teststatus=1" patchlist
grep "teststatus=125" patchlist
not grep "teststatus=0" patchlist
