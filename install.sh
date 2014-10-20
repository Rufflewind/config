#!/bin/sh
set -e

# Be sure to configure makepkg and pacman before doing all this

# sudo pacman-key -r KEYID
# sudo pacman-key --lsign-key KEYID

# Note that `xterm` is needed for `xmonad` under the default settings.  Skype
# comes from the `multilib` repository (which allows 32-bit to be run on
# 64-bit).

# libxkbcommon-x11 is required by matplotlib

sudo pacman -Syu --needed \
    rxvt-unicode xterm \
    alsa-utils dnsutils mlocate patchutils whois unzip zip expac yajl \
    dhclient dialog wpa_supplicant \
    aspell-en bash-completion git openssh tk\
    emacs auctex emacs-haskell-mode emacs-lua-mode emacs-python-mode \
    htop irssi lynx mutt \
    xorg xorg-xmessage \
    xfce4 xfce4-goodies \
    xmonad xmonad-contrib fvwm xcompmgr feh dmenu transset-df \
    dzen2 conky xbindkeys xcursor-themes xscreensaver \
    chromium gnome-keyring firefox vlc \
    jre7-openjdk skype \
    evince texlive-langgreek texlive-most \
    cabal-install clang doxygen ghc python2 python3 \
    python-numpy python-scipy python-matplotlib libxkbcommon-x11 \
    python-statsmodels python-pandas \
    gimp inkscape \
    infinality-bundle infinality-bundle-multilib ibfonts-meta-extended \
    otf-fira-mono-ibx otf-fira-sans-ibx otf-inconsolataz14-ibx ttf-lato-ibx \
    ttf-oxygen-ibx

# install pacaur:
# - cower, expac is for pacaur
# - yajl is for cower
#
# curl -L https://aur.archlinux.org/packages/co/cower/cower.tar.gz | tar xzf -
# curl -L https://aur.archlinux.org/packages/pa/pacaur/pacaur.tar.gz | tar xzf -

# pacaur -S ttf-envy-code-r

sudo pip install ggplot sudo pacman -S husl
