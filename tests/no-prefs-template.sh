#!/usr/bin/env bash
#
# Tests for `--[with|no]-prefs-template` options
#

. lib

has_template() {
  grep -v '^#' "$1" | grep -v '^\s*$'
}


#########
## Tests for `init` command
#

rm -rf temp1
mkdir temp1
cd temp1
darcs init
test -f _darcs/prefs/boring
has_template _darcs/prefs/boring # by default boring file is filled with template
test -f _darcs/prefs/binaries
has_template _darcs/prefs/binaries # by default binaries file is filled with template
cd ..

rm -rf temp1
mkdir temp1
cd temp1
darcs init --with-prefs-template
test -f _darcs/prefs/boring
has_template _darcs/prefs/boring # boring file is filled with template
test -f _darcs/prefs/binaries
has_template _darcs/prefs/binaries # binaries file is filled with template
cd ..

rm -rf temp1
mkdir temp1
cd temp1
darcs init --no-prefs-template
test -f _darcs/prefs/boring
not has_template _darcs/prefs/boring # boring file is not filled with template
test -f _darcs/prefs/binaries
not has_template _darcs/prefs/binaries # binaries file not is filled with template
cd ..


#########
## Tests for `clone` command
#

rm -rf temp1
mkdir temp1
cd temp1
darcs init
cd ..

rm -rf temp2
darcs clone temp1 temp2
cd temp2
test -f _darcs/prefs/boring
has_template _darcs/prefs/boring # by default boring file is filled with template
test -f _darcs/prefs/binaries
has_template _darcs/prefs/binaries # by default binaries file is filled with template
cd ..

rm -rf temp2
darcs clone --with-prefs-template temp1 temp2
cd temp2
test -f _darcs/prefs/boring
has_template _darcs/prefs/boring # boring file is filled with template
test -f _darcs/prefs/binaries
has_template _darcs/prefs/binaries # binaries file is filled with template
cd ..

rm -rf temp2
darcs clone --no-prefs-template temp1 temp2
cd temp2
test -f _darcs/prefs/boring
not has_template _darcs/prefs/boring # boring file is not filled with template
test -f _darcs/prefs/binaries
not has_template _darcs/prefs/binaries # binaries file not is filled with template
cd ..



rm -rf temp1 temp2
