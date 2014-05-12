#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# disable touchpad if mouse is plugged in
touchpad-ctl

# start compositor (with black screen)
hsetroot -solid "#000000"
xcompmgr -c &

# set wallpaper
hsetroot -center pictures/wallpapers.DUPE/wallpaper-726807.jpg

xsetroot -cursor_name left_ptr
xrdb ~/.Xresources &
exec xmonad
