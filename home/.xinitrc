#!/bin/sh

exec 2>"$HOME/.cache/xinitrc.log"

# source configuration scripts
for d in /etc/X11/xinit/xinitrc.d \
         "${XDG_CONFIG_HOME-${HOME}/.config}/xinitrc.d"; do
    [ ! -d "${d}" ] || for f in "${d}/"*; do
        [ ! -f "${f}" ] || . "${f}"
    done
done

# disable touchpad if mouse is plugged in
touchpad-ctl

setxkbmap -option compose:caps

# needed for ~/.XCompose to work on GTK
GTK_IM_MODULE=xim
XMODIFIERS="@im=none"
export GTK_IM_MODULE XMODIFIERS

session=${2:-${1:-xmonad}}
case ${session} in

    gnome) exec gnome-session;;

    i3)    exec i3;;

    xfce)  exec startxfce4;;

    xmonad)

        # to configure the wallpaper, use `feh --bg-center` or `feh --bg-fill`,
        # which automatically caches the settings in `~/.fehbg`
        [ ! -f "${HOME}/.fehbg" ] || "${HOME}/.fehbg"

        # start screen saver
        xscreensaver -no-splash &

        xrdb "${HOME}/.Xresources" && xsetroot -cursor_name left_ptr &
        xbindkeys &
        { sleep 1; xset dpms; } &
        exec xmonad

        ;;

esac

printf >&2 "%s\n" "$0: invalid session: ${session}"
exit 1
