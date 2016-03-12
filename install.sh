#!/bin/bash
set -eu

# Be sure to configure makepkg and pacman before doing all this
#
#     sudo pacman-key -r KEYID
#     sudo pacman-key --lsign-key KEYID
#
# when running `makepkg` you may also need to run this:
#
#     gpg --recv-keys KEYID

sudo pacman -Sy --needed --noconfirm archlinux-keyring
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
    sudo pacman -S --needed --noconfirm yajl
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
    sudo pacman -S --needed --noconfirm expac
    makepkg
    sudo pacman -U *.pkg.tar.xz
)

# Note that `xterm` is needed for `xmonad` under the default settings.  Skype
# comes from the `multilib` repository (which allows 32-bit to be run on
# 64-bit).

sudo patch -d/ -N -p0 -r- <root/etc/pacman.conf.patch || :

# libxkbcommon-x11 is wanted by matplotlib;
# python-{numpy,scipy,pandas} are dependencies of python-statsmodels

pkgs=(

    # system
    alsa-utils
    mlocate
    ntfs-3g
    zsh zsh-syntax-highlighting
    # jmtpfs # optional

    # files
    patchutils
    xz
    zip unzip

    # Pacman
    namcap
    pkgbuild-introspection

    # network
    netctl dialog wpa_actiond wpa_supplicant
    bind-tools
    net-tools
    netcat
    whois
    ntp
    openssh
    rsync
    sshfs

    # miscellaneous
    aspell-en
    bash-completion

    # Emacs
    emacs
    emacs-haskell-mode
    emacs-lua-mode
    emacs-python-mode
    auctex

    # terminal applications
    htop
    irssi
    lynx
    mutt

    # development
    git
    #git-annex-bin # not included as the checksum often fails
    doxygen
    gdb
    strace
    valgrind
    jre7-openjdk
    # openblas-lapack # optional

    ## TeX
    texlive-most
    texlive-langgreek

    ## C and C++
    clang

    ## Haskell
    cabal-install
    ghc

    ## Python
    python2
    python3
    python-gobject
    python-google-api-python-client
    python-statsmodels
    python-matplotlib
    libxkbcommon-x11

    # X
    xorg
    xfce4-terminal
    xterm

    ## X utilities
    xbindkeys
    xscreensaver
    feh
    maim slop

    ## XFCE
    xfce4
    xfce4-goodies

    ## Xmonad
    xmonad
    xmonad-contrib
    xmobar
    xorg-xmessage
    dmenu

    ## visual effects
    fvwm
    transset-df
    xcompmgr
    xcursor-themes

    ## graphical applications
    chromium
    evince
    firefox
    gimp
    inkscape
    vlc

    ## graphical utilities
    gnome-keyring
    kdiff3
    rdesktop

    ## typeface
    infinality-bundle
    infinality-bundle-multilib
    ibfonts-meta-extended
    otf-fira-mono-ibx
    otf-fira-sans-ibx
    otf-inconsolatazi4-ibx
    ttf-lato-ibx
    ttf-oxygen-ibx
    # ttf-andale-mono
    # ttf-envy-code-r
    # ttf-monaco
    # ttf-lato
    # ttf-palatino

    # Yubikey
    # libu2f-host
    # libusb-compat
    # pcsclite
    # yubikey-personalization-gui

)
pacaur -S --needed --noconfirm "${pkgs[@]}"

sudo systemctl enable ntpd
# sudo systemctl enable pcscd # required?
