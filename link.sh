#!/bin/sh

cd `dirname "$0"`
PWD=`pwd`

link_home() {
    P="$1"
    SOURCE="$PWD/home/$P"
    TARGET="$HOME/$P"
    mkdir -p `dirname "$TARGET"`
    ln -is "$SOURCE" "$TARGET"
}

link_home .xbindkeysrc
link_home .xinitrc
link_home .xmonad/xmonad.hs
link_home .Xresources
link_home bin

sudo cp -ir root /

# sudo mkdir -p /root/.emacs.d
# sudo cp -r ~/.emacs.d/init.el \
#            ~/.emacs.d/elisp \
#            /root/.emacs.d/
