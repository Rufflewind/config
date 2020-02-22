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

        {
            xrdb "${HOME}/.Xresources"
            xsetroot -cursor_name left_ptr
            xscreensaver -no-splash &
        } &

        dunst &
        xbindkeys &
        { sleep 1; xset dpms; } &

        # remove the cached binary if it doesn't appear runnable
        # (this happens if the underlying shared libs get upgraded)
        "${HOME}/.xmonad/xmonad-x86_64-linux" --version >/dev/null 2>&1 || {
            rm -f "${HOME}/.xmonad/xmonad-x86_64-linux"
        }
        exec xmonad

        ;;

esac

printf >&2 "%s\n" "$0: invalid session: ${session}"
exit 1
