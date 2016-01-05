#!/bin/sh
set -eu

# Be sure to configure makepkg and pacman before doing all this
#
#     sudo pacman-key -r KEYID
#     sudo pacman-key --lsign-key KEYID
#
# when running `makepkg` you may also need to run this:
#
#     gpg --recv-keys KEYID

sudo pacman -Syu --noconfirm

pacman -Q cower >/dev/null 2>&1 || (
    dir=${TMPDIR:-/tmp}/$$
    rm -fr "$dir"
    mkdir "$dir"
    cd "$dir"
    curl -L https://aur.archlinux.org/cgit/aur.git/snapshot/cower.tar.gz | tar xzf -
    cd cower
    ls -l
    cat PKGBUILD
    echo "Inspect the package and then type 'exit 0' to continue ..."
    "$SHELL"
    gpg --recv-keys 1eb2638ff56c0c53
    sudo pacman -S yajl
    makepkg
    sudo pacman -U *.pkg.tar.xz
)
pacman -Q pacaur >/dev/null 2>&1 || (
    dir=${TMPDIR:-/tmp}/$$
    rm -fr "$dir"
    mkdir "$dir"
    cd "$dir"
    curl -L https://aur.archlinux.org/cgit/aur.git/snapshot/pacaur.tar.gz | tar xzf -
    cd pacaur
    ls -l
    cat PKGBUILD
    echo "Inspect the package and then type 'exit 0' to continue ..."
    "$SHELL"
    sudo pacman -S expac
    makepkg
    sudo pacman -U *.pkg.tar.xz
)

# Note that `xterm` is needed for `xmonad` under the default settings.  Skype
# comes from the `multilib` repository (which allows 32-bit to be run on
# 64-bit).

sudo patch -d/ -N -p0 -r- <root/etc/pacman.conf.patch || :

# libxkbcommon-x11 is wanted by matplotlib;
# python-{numpy,scipy,pandas} are dependencies of python-statsmodels
pacaur -S --needed --noconfirm \
    rxvt-unicode xterm maim slop ntp \
    alsa-utils bind-tools gdb mlocate ntfs-3g patchutils whois unzip zip \
    dialog namcap net-tools pkgbuild-introspection wpa_actiond wpa_supplicant \
    aspell-en bash-completion git netcat openssh rsync sshfs strace tk \
    emacs auctex emacs-haskell-mode emacs-lua-mode emacs-python-mode \
    zsh zsh-syntax-highlighting \
    htop irssi lynx mutt \
    xorg xorg-xmessage \
    xfce4 xfce4-goodies \
    xmonad xmonad-contrib fvwm xcompmgr feh dmenu transset-df \
    xmobar xbindkeys xcursor-themes xscreensaver \
    chromium gnome-keyring firefox vlc \
    evince kdiff3 texlive-langgreek texlive-most \
    cabal-install clang doxygen ghc \
    python2 python3 \
    python-google-api-python-client python-statsmodels \
    python-matplotlib libxkbcommon-x11 \
    gimp inkscape jre7-openjdk rdesktop \
    infinality-bundle infinality-bundle-multilib ibfonts-meta-extended \
    otf-fira-mono-ibx otf-fira-sans-ibx otf-inconsolatazi4-ibx ttf-lato-ibx \
    ttf-oxygen-ibx

# optional things:
#
# jmtpfs
# openblas-lapack
# ttf-envy-code-r
# z3-git

# git-annex-bin # not included as the checksum often fails
# ttf-andale-mono
# ttf-monaco
# ttf-lato
# ttf-palatino

sudo systemctl enable ntpd
