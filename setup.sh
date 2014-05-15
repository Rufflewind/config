#!/bin/sh
set -e
echo >&2 "Don't just execute this file.  Read it carefully!"
[ "$1" = --i-know-what-im-doing ] || exit 1
# Installation
# ============
#
# This is meant to be a supplementary to the ArchLinux docs, containing some
# of the choices that I made during the process (and occasionally, helpful
# pointers).
#
# Preparing the boot disk
# -----------------------
#
# This first part assumes that you begin on Windows and need to burn the ISO
# to a flash drive (as I originally did).  If your circumstances are
# different, visit the official docs instead:
#
#     https://wiki.archlinux.org/index.php/Beginners%27_guide
#             #Prepare_the_latest_installation_medium
#
# First, get the "dual" ISO from the download page:
#
#     https://archlinux.org/download
#
# Verify the signature.  Burn the ISO to a flash drive using USBWriter:
#
#     http://sourceforge.net/projects/usbwriter
#
# Then find your way into the BIOS and boot it from the disk.  You are
# provided a `zsh` shell with some basic tools.  There's no Emacs so you're
# going to have to use either `nano` or `vi`.
#
# Setting up Internet
# -------------------
#
# You'll need an Internet connection.  If you can, prefer wired over wireless
# as they seem to be lot less frustrating to set up.  Follow the steps in:
#
#     https://wiki.archlinux.org/index.php/Beginners%27_guide
#             #Establish_an_internet_connection
#
# In my situation, `dhcpcd` didn't work so I had to manually set my own IP
# address.  (And I couldn't get wireless to work at all [TODO].)
#
# Partitioning
# ------------
#
#     https://wiki.archlinux.org/index.php/Beginners%27_guide
#             #Prepare_the_storage_drive
#
# I picked `fdisk` since I just wanted a single, good ol' MBR partition.  The
# interface of the program is pretty simple and I think the docs are pretty
# self-explanatory.
#
# Afterwards, mount the partition(s) somewhere, e.g.
#
#     mount /dev/sd((DRIVE))((PARTITION)) /mnt
#
# where `((DRIVE))` is a letter and `((PARTITION))` is a number.
#
# Installing the base
# -------------------
#
# Run the `pacstrap` script to install the base packages:
#
#     pacstrap /mnt base base-devel
#
# (Add `-i` if you want interactive confirmations.)
#
# Generate the `fstab`
# --------------------
#
# Be sure to check if everything is OK:
#
#     genfstab >>/mnt/etc/fstab -U -p /mnt
#     less /mnt/etc/fstab
#
# Installing the bootloader
# -------------------------
#
# You could use GRUB but here we'll use the simpler one: syslinux.
#
#     pacman -S syslinux
#     syslinux-install_update -i -a -m
#
# Now edit `/boot/syslinux/syslinux.cfg` (there are TWO lines that look like this):
#
#     APPEND root=/dev/sd((DRIVE))((PARTITION)) rw
#
# of the corresponding system partition, of course.
#
# Configuring the system
# ----------------------
#
# Now we can log into the new system:
#
#     arch-chroot /mnt /bin/bash
#
# There are a couple things to set up here (check the wiki for details):
#
#  0. Assuming your `((LANG))` is `en_US.UTF-8`...
#
#  1. Uncomment `((LANG)) UTF-8` in `/etc/locale.gen`.  Use `C-w` to
#     search in `nano`.
#
#  2. Then run:
#
#         locale-gen
#         export $LANG=((LANG))
#         echo >/etc/locale.conf LANG=$LANG
#
#  3. Set the time zone (you'll find them under `/usr/share/zoneinfo/`) and
#     hardware clock:
#
#         ln -s /usr/share/zoneinfo/((ZONE))/((SUBZONE)) /etc/localtime
#         hwclock --systohc --utc
#
#  4. Set the hostname:
#
#         echo >/etc/hostname ((HOSTNAME))
#
#  5. Add the hostname to `/etc/hosts` immediately after `localhost` for the
#     line corresponding to `127.0.0.1`.  See this link if you're not sure
#     what I mean:
#
#         https://wiki.archlinux.org/index.php/Beginners%27_guide#Hostname
#
#  6. Set up the Internet again.  See wiki.
#
# Disabling root
# --------------
#
# Uncomment the line that says "%wheel ALL=(ALL) ALL"
#
#     visudo
#
# This way, anyone in the `wheel` group can use `sudo`.  Now, create the
# non-root user `((USER))`:
#
#     USER=((USER))
#     useradd -m -G wheel -s /bin/bash $USER
#     passwd $USER
#
# Then you can disable root login
#
#     passwd -l root
#
# Warning: do NOT make the root account expire via `usermod -e 1 root`.  If
# you do this then you will break some programs that require the root account
# to be active, including `usermod` itself!  If you do happen to do this, edit
# `/etc/shadow` and remove the expiration time (if you don't have a sudoer for
# this, then use `chroot` with a boot disk as you had done previously).
#
# Fun stuff
# ---------
#
# OK, so the basic system is ready, so try booting into it.
#
#     shutdown 0
#
# Remove the flash drive and start the computer.
#
# ----------------------------------------------------------------------------



# Change some build settings in `/etc/makepkg.conf`:
#
#   - Edit the part in `CFLAGS` to say `-march=native` and remove `-mtune=generic`.
#   - Edit `CXXFLAGS` to simply `"${CFLAGS}"`.
#
# Open `/etc/pacman.conf` and uncomment the sections for `[extra]`,
# `[community]`, and `[multilib]`.


# Install some basic stuff

sudo pacman -Syu

sudo pacman -S \
    unzip zip \
    openssh \
    emacs \
    lynx \
    mutt \
    irssi \
    dhclient dialog wpa_supplicant

sudo pacman -S \
    xorg xorg-xmessage \
    xfce4 xfce4-goodies \
    xmonad xmonad-contrib fvwm xcompmgr transset-df \
    xcursor-themes \
    rxvt-unicode xterm

sudo pacman -S \
    alsa-utils \
    chromium gnome-keyring \
    firefox \
    jre7-openjdk \
    skype \
    vlc

sudo pacman -S \
    texlive-most \
    emacs-haskell-mode emacs-lua-mode emacs-python-mode

# Note that `xterm` is needed for `xmonad` under the default settings.  Skype
# comes from the `multilib` repository (which allows 32-bit to be run on
# 64-bit).

# Drivers
# -------
#
#     sudo pacman -S xf86-video-ati    # for ATI video cards

# GitHub keys
# -----------
#
# First generate the key with `ssh-keygen`.  Then go to:
#
#     https://github.com/settings/ssh
#
# in `lynx` and copy the SSH public key at `~/.ssh/id_rsa.pub` into the textbox
# (via the shortcut `C-x i`).
