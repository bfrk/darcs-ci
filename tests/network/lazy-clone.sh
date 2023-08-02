#!/usr/bin/env bash

. lib
. httplib

rm -rf tabular
unpack_testdata tabular
serve_http # sets baseurl

rm -rf temp temp2
darcs clone --lazy $baseurl/tabular temp
darcs clone --lazy temp temp2

cd temp2
test ! -f _darcs/patches/0000005705-178beaf653578703e32346b4d68c8ee2f84aeef548633b2dafe3a5974d763bf2
darcs log -p 'Initial version' -v | cat
test -f _darcs/patches/0000005705-178beaf653578703e32346b4d68c8ee2f84aeef548633b2dafe3a5974d763bf2
cd ..

# test if we can unapply patches after a tag
rm -rf temp4
darcs clone --lazy $baseurl/tabular temp4 --tag '^0.1$'
darcs log --repo=temp4 # to get all inventories
darcs log --repo=temp # to get all inventories
finish_http $PWD
# and that the log -v output is correct
darcs log -v --repo=temp4 > LOG
if grep no-cache $HOME/.darcs/defaults; then
  expected_unavailable=33
else
  # With cache, the patch 'initial version' is available
  # because of the darcs log command in temp2 above.
  expected_unavailable=32
fi
test $(grep -c 'this patch is unavailable' LOG) = $expected_unavailable

darcs log -v --repo=temp
