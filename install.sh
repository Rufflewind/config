#!/bin/sh
set -e

# Be sure to configure makepkg and pacman before doing all this
#
#     sudo pacman-key -r KEYID
#     sudo pacman-key --lsign-key KEYID
#
# when running `makepkg` you may also need to run this:
#
#     gpg --recv-keys KEYID

# Note that `xterm` is needed for `xmonad` under the default settings.  Skype
# comes from the `multilib` repository (which allows 32-bit to be run on
# 64-bit).

# libxkbcommon-x11 is required by matplotlib

sudo pacman -Syu --needed \
    rxvt-unicode xterm \
    alsa-utils bind-tools gdb mlocate patchutils whois unzip zip expac yajl \
    dhclient dialog net-tools wpa_supplicant \
    aspell-en bash-completion git netcat openssh sshfs strace tk \
    emacs auctex emacs-haskell-mode emacs-lua-mode emacs-python-mode \
    zsh zsh-syntax-highlighting \
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
    otf-fira-mono-ibx otf-fira-sans-ibx otf-inconsolatazi4-ibx ttf-lato-ibx \
    ttf-oxygen-ibx

# install pacaur:
# - cower, expac is for pacaur
# - yajl is for cower
#
# curl -L https://aur.archlinux.org/packages/co/cower/cower.tar.gz | tar xzf -
# curl -L https://aur.archlinux.org/packages/pa/pacaur/pacaur.tar.gz | tar xzf -

'
pacaur -S \
    emacs-rust-mode \
    git-annex-bin \
    jmtpfs \
    openblas-lapack \
    ttf-andale-mono \
    ttf-envy-code-r \
    ttf-lato \
    ttf-monaco \
    ttf-palatino \
    twurl \
    z3-git
'
