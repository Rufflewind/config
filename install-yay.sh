#!/bin/sh
# Self-contained script for bootstrapping an arch system with yay.

inspect_pkg() {
    ls -l
    cat PKGBUILD
    printf '\x1b[33;1m%s\x1b[30;42m%s\x1b[0m\x1b[33;1m%s\x1b[0m\n' \
           "Inspect the package and then type " " exit 0 " " to continue ..."
    "$SHELL" || exit
}

pacman -Q yay >/dev/null 2>&1 || {
    tmp=`mktemp -d`
    cd "$tmp"
    pkg=yay
    curl -L "https://aur.archlinux.org/cgit/aur.git/snapshot/$pkg.tar.gz" | tar xzf -
    cd "$pkg"
    inspect_pkg
    makepkg -s
    sudo pacman -U --needed --noconfirm "$pkg-*.tar.xz"
}
