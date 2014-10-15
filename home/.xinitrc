#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# disable touchpad if mouse is plugged in
touchpad-ctl

session=${2:-${1:-xmonad}}
case $session in

    xfce) exec startxfce4;;

    xmonad)

        # start compositor (with black screen)
        hsetroot -solid "#000000"
        # the fading also works around a bug with urxvt where it shows the
        # wrong transparency initially
        xcompmgr -cf -D 5 -I 0.1 -O 0.1 &

        # set wallpaper
        feh --bg-center ~/.wallpaper.png

        # start screen saver
        xscreensaver -no-splash &

        xrdb ~/.Xresources && xsetroot -cursor_name left_ptr &
        xbindkeys &
        { sleep 1; xset dpms; } &
        exec xmonad

        ;;

esac

printf >&2 "%s\n" "$0: invalid session: $session"
exit 1
