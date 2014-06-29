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
hsetroot -full ~/.wallpaper.png

# start screen saver
xscreensaver -no-splash &

xrdb ~/.Xresources && xsetroot -cursor_name left_ptr &
xbindkeys &
exec xmonad
