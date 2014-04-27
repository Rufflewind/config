#!/bin/sh

sudo mkdir -p /root/.emacs.d
sudo cp    ~/.emacs.d/init.el /root/.emacs.d/init.el
sudo cp -r ~/.emacs.d/elisp   /root/.emacs.d/

sudo cp bin/touchpad-ctl /bin/touchpad-ctl
sudo cp boot/etc/udev/rules.d/01-touchpad.rules /etc/udev/rules.d/
