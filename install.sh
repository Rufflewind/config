#!/bin/sh
set -e

# Be sure to configure makepkg and pacman before doing all this

# Note that `xterm` is needed for `xmonad` under the default settings.  Skype
# comes from the `multilib` repository (which allows 32-bit to be run on
# 64-bit).

sudo pacman -Syu --needed \
    rxvt-unicode xterm \
    alsa-utils dnsutils patchutils whois unzip zip expac yajl \
    dhclient dialog wpa_supplicant \
    openssh bash-completion \
    emacs auctex emacs-haskell-mode emacs-lua-mode emacs-python-mode \
    irssi lynx mutt \
    xorg xorg-xmessage \
    xfce4 xfce4-goodies \
    xmonad xmonad-contrib fvwm xcompmgr feh dmenu transset-df \
    dzen2 conky xcursor-themes \
    chromium gnome-keyring firefox vlc \
    jre7-openjdk skype \
    texlive-langgreek texlive-most \
    clang ghc python2 python3 \
    gimp inkscape

# install pacaur:
# - cower, expac is for pacaur
# - yajl is for cower
curl -L https://aur.archlinux.org/packages/co/cower/cower.tar.gz | tar xzf -
curl -L https://aur.archlinux.org/packages/pa/pacaur/pacaur.tar.gz | tar xzf -
