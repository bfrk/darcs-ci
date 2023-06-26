#!/usr/bin/env bash

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init

date > date.t
date > date_moved.t

write_buggy_pending () {
cat > _darcs/patches/pending <<EOF
{
addfile ./date.t
addfile ./date.t
addfile ./date_moved.t
move ./date.t ./date_moved.t
}
EOF
}

write_buggy_pending

# darcs log should be unaffected by pending, even with -v
darcs log -v >/dev/null

write_buggy_pending

# darcs check should detect a broken pending
not darcs check 2>&1 | tee out
grep -i 'broken pending' out

write_buggy_pending

# darcs whatsnew should fail
not darcs whatsnew 2>&1 | tee out
grep -i 'cannot apply pending' out
# and issue a recommendation about repair
grep -i 'darcs repair' out

# darcs revert should fail
not darcs revert -a 2>&1 | tee out
grep -i 'cannot apply pending' out
# and issue a recommendation about repair
grep -i 'darcs repair' out

# darcs revert should fail
not darcs record -a -m foo 2>&1 | tee out
grep -i 'cannot apply pending' out
# and issue a recommendation about repair
grep -i 'darcs repair' out

write_buggy_pending

# we should be able to successfully repair pending
darcs repair -v 2>&1 | tee out
grep -i 'repaired pending' out

darcs whatsnew
darcs check

write_buggy_pending

# final repair, quiet
darcs repair -q 2>&1 | not grep .

darcs check
darcs record -a -m foo
darcs check

cd ..
