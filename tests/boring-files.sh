#!/usr/bin/env bash

#######
## Check that _darcs/ptefs/boring file is ALWAYS used
#
rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch test.txt

echo > _darcs/prefs/boring
darcs wh -l | grep test.txt # Empty boring file - new file is reported.

echo test.txt > _darcs/prefs/boring
darcs wh -l | grep -v test.txt # Pattern in the repository private boring file is used.

touch .boring
darcs setpref boringfile .boring
darcs record -am"setpref boringfile"
darcs wh -l | grep -v test.txt # Pattern in the repository private boring file is used
                               # despite the fact that there is an explicitly defined
                               # boringfile.

echo > _darcs/prefs/boring
echo test.txt > .boring
darcs wh -l | grep -v test.txt # Explicitly defined boringfile is used

echo > _darcs/prefs/boring
echo > .boring
darcs wh -l | grep test.txt # Both explicitly defined boringfile and repository private
                            # boring file are empty and do not mask new file.


rm -rf temp1
