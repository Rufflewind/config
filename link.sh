#!/bin/sh
set -e

cd `dirname "$0"`
PWD=`pwd`

link_home() {
    SOURCE="$PWD/home/$1"
    TARGET_DIR=`dirname "$HOME/$1"`/
    mkdir -p "$TARGET_DIR"
    ln -is "$SOURCE" "$TARGET_DIR"
}

link_root() {
    SOURCE="$PWD/root/$1"
    TARGET_DIR=`dirname "/$1"`/
    sudo mkdir -p "$TARGET_DIR"
    sudo cp -i "$SOURCE" "$TARGET_DIR"
}

link_home .xbindkeysrc
link_home .xinitrc
link_home .xmonad/xmonad.hs
link_home .Xresources
link_home bin

link_root bin/touchpad-ctl
link_root etc/iptables/iptables.rules
link_root etc/udev/rules.d/01-touchpad.rules

# sudo mkdir -p /root/.emacs.d
# sudo cp -r ~/.emacs.d/init.el \
#            ~/.emacs.d/elisp \
#            /root/.emacs.d/
