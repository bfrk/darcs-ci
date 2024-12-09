#!/usr/bin/env bash

. ./lib

# Print some stuff out for debugging if something goes wrong:
cat ./env >&2
echo HOME="$HOME" >&2
pwd >&2
echo CWD="$CWD" >&2
which darcs >&2
command -v darcs >&2

# Check things that should be true when all the testscripts run

test -f "$HOME/lib"
password="AKARABNADABARAK-KARABADANKBARAKA"
grep $password "$HOME/test" || grep $password "$HOME/harness.sh"

if echo $OS | grep -i windows; then
    if echo $OSTYPE | grep -i cygwin ; then
        real=$(cygpath -w $(command -v darcs.exe) | sed -e 's,\\,/,g' | tr -s '[:upper:]' '[:lower:]')
    else
        real=$(cmd //c echo $(command -v darcs.exe) | sed -e 's,\\,/,g' | tr -s '[:upper:]' '[:lower:]')
    fi
    wanted=$(echo "$DARCS" | sed -e 's,\\,/,g' | tr -s '[:upper:]' '[:lower:]')
    test "$real" = "$wanted"
else
    command -v darcs | fgrep "$DARCS"
fi
exit 1
