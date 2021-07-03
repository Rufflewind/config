#!/bin/sh
# https://wiki.archlinux.org/title/Install_Arch_Linux_from_existing_Linux#Method_A:_Using_the_bootstrap_image_(recommended)
# 1. First follow the steps there to download and verify the image.
# 2. Un-tar the image to /opt/archlinux with --strip-components=1.

root=/opt/archlinux

[ -f "$root/etc/fyl-arch-chroot-initialization.ok" ] || sudo tee "$root/etc/profile.d/00-fyl-arch-chroot-initialization.sh" <<"EOF" >/dev/null
echo 'Server = https://mirror.rackspace.com/archlinux/$repo/os/$arch' >/etc/pacman.d/mirrorlist
pacman-key --init
pacman-key --populate archlinux
touch /etc/fyl-arch-chroot-initialization.ok
rm /etc/profile.d/00-fyl-arch-chroot-initialization.sh
EOF

mountpoint -q "$root" || sudo mount --bind "$root" "$root"
exec sudo "$root/bin/arch-chroot" "$root" /bin/bash -l
