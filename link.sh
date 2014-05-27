#!/bin/sh

cd `dirname "$0"`
PWD=`pwd`

link_home() {
    P="$1"
    SOURCE="$PWD/home/$P"
    TARGET="$HOME/$P"
    mkdir -p `dirname "$TARGET"`
    ln -s "$SOURCE" "$TARGET"
}
link_root() {
    P="$1"
    SOURCE="$PWD/root/$P"
    TARGET="/$P"
    sudo mkdir -p `dirname "$TARGET"`
    sudo cp "$SOURCE" "$TARGET"
}

link_home .xbindkeysrc
link_home .xinitrc
link_home .Xresources
link_home .xmonad/xmonad.hs
link_home bin

# sudo mkdir -p /root/.emacs.d
# sudo cp    ~/.emacs.d/init.el /root/.emacs.d/
# sudo cp -r ~/.emacs.d/elisp   /root/.emacs.d/

link_root bin/touchpad-ctl
link_root boot/etc/udev/rules.d/01-touchpad.rules
