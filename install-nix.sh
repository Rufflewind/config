#!/bin/sh
set -eu
nixver=1.10
prefix=$HOME/.local
pacman -Q bzip2 perl-dbd-sqlite perl-www-curl >/dev/null 2>&1 ||
    sudo pacman -S --needed bzip2 perl-dbd-sqlite perl-www-curl
mkdir -p "$prefix/src"
cd "$prefix/src"
[ -d nix-$nixver ] ||
    curl -L https://nixos.org/releases/nix/nix-$nixver/nix-$nixver.tar.xz | tar -xJ
cd nix-$nixver
./configure --prefix="$prefix" \
            --with-store-dir="$prefix/nix/store" \
            --localstatedir="$prefix/nix/var"
make -j`nproc` install
