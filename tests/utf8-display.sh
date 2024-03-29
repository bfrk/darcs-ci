#!/bin/sh -e
##
## Basic test of displaying metadata in the UTF8 locale
##
## Copyright (C) 2014 Ganesh Sittampalam
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

switch_to_utf8_locale

#is now the default, must use DARCS_ESCAPE_8BIT to disable
#export DARCS_DONT_ESCAPE_8BIT=1

rm -rf R
mkdir R
cd R

darcs init
echo 'Société nationale des chemins de fer français' > f
darcs rec -lam 'creació de a i b' | grep 'creació de a i b'

darcs changes | grep 'creació de a i b'

darcs annotate f | grep 'creació de a i b'
darcs annotate f | grep 'Société nationale des chemins de fer français'
