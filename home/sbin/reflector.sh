#!/bin/sh
tmp=`mktemp`
reflector --verbose --country="United States" --score=10 --sort=rate | tee "$tmp"
cat >&2 <<EOF
========================================
To accept these changes, run:

    sudo cp "$tmp" /etc/pacman.d/mirrorlist && rm "$tmp"

EOF
