#!/usr/bin/env bash

. lib
. sshlib

init_remote_repo R

rm -rf R
darcs clone --remote-darcs=xyzabc "${REMOTE}:${REMOTE_DIR}/R" --debug 2>LOG
grep '"ssh" .* "xyzabc"' LOG
not grep '"ssh" .* "darcs"' LOG
