#!/bin/sh
cd `dirname "$0"`
PWD=`pwd`

mkdir -p ~/.xmonad
ln -fs "$PWD"/home/.Xresources       ~/
ln -fs "$PWD"/home/.xinitrc          ~/
ln -fs "$PWD"/home/.xmonad/xmonad.hs ~/

sudo mkdir -p /root/.emacs.d
sudo cp    ~/.emacs.d/init.el /root/.emacs.d/
sudo cp -r ~/.emacs.d/elisp   /root/.emacs.d/

sudo cp bin/touchpad-ctl                        /bin/
sudo cp boot/etc/udev/rules.d/01-touchpad.rules /etc/udev/rules.d/
