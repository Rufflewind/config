#!/bin/sh

# source configuration scripts from:
#
#   - /etc/X11/xinit/xinitrc.d
#   - ~/.config/xinitrc.d
#
for d in /etc/X11/xinit/xinitrc.d "$HOME/.config/xinitrc.d"
do
    if [ ! -d "$d" ]
    then continue
    fi
    for f in "$d/"*
    do
        if [ -f "$f" ]
        then  . "$f"
        fi
    done
    unset f
done
unset d

# disable touchpad if mouse is plugged in
touchpad-ctl

setxkbmap -option compose:caps

# needed for ~/.XCompose to work on GTK
GTK_IM_MODULE=xim
XMODIFIERS="@im=none"
export GTK_IM_MODULE XMODIFIERS

session=${2:-${1:-xmonad}}
case $session in

    gnome) exec gnome-session;;

    i3)    exec i3;;

    xfce)  exec startxfce4;;

    xmonad)

        # start compositor (with black screen)
        # FIXME: temporarily disabled due to bugs
        if false
        then
            hsetroot -solid "#000000"
            # the fading also works around a bug with urxvt where it shows the
            # wrong transparency initially
            xcompmgr -cf -D 5 -I 0.1 -O 0.1 &
        fi

        # to configure the wallpaper, use `feh --bg-center` or `feh --bg-fill`,
        # which automatically caches the settings in `~/.fehbg`
        if [ -f "$HOME/.fehbg" ]
        then  . "$HOME/.fehbg"
        fi

        # start screen saver
        xscreensaver -no-splash &

        xrdb "$HOME/.Xresources" && xsetroot -cursor_name left_ptr &
        xbindkeys &
        { sleep 1; xset dpms; } &
        exec xmonad

        ;;

esac

printf >&2 "%s\n" "$0: invalid session: $session"
exit 1
