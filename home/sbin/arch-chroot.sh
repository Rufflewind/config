#!/bin/bash
set -eux

# ------------------------------------------------------------------------
# Configurable parameters
# ------------------------------------------------------------------------
# Documentation:
# https://wiki.archlinux.org/title/Install_Arch_Linux_from_existing_Linux#Method_A:_Using_the_bootstrap_image_(recommended)

# Find the latest version here: https://mirror.rackspace.com/archlinux/iso/
version=2021.09.01

# Arbitrary name for the chroot instance
instance=0

mountpoint=/opt/arch/$instance
internal=/opt/arch-chroot
base=$internal/images/$version

# ------------------------------------------------------------------------

download_file() {
    local out=$1
    local url=$2
    if [ -f "$out" ]; then
        return
    fi
    sudo mkdir -p "$(dirname "$out")"
    curlx -LSfs "$url" | sudo tee "$out.tmp" >/dev/null
    sudo mv "$out.tmp" "$out"
}

download_and_unpack_image() {
    local version=$1
    local base=$2
    local tar=$base/archlinux-bootstrap-$version-x86_64.tar.gz
    local out=$base/.ok
    if [ -f "$out" ]; then
        return
    fi
    download_file "$tar" "https://mirror.rackspace.com/archlinux/iso/$version/archlinux-bootstrap-$version-x86_64.tar.gz"
    download_file "$tar.sig" "https://mirror.rackspace.com/archlinux/iso/$version/archlinux-bootstrap-$version-x86_64.tar.gz.sig"
    gpg --verify "$tar.sig"
    sudo tar -xz -C "$base" -f "$tar"
    sudo touch "$out"
}

mount_image() {
    local mountpoint=$1
    local base=$2
    local overlay=$3
    sudo mkdir -p "$overlay/upper" "$overlay/work" "$mountpoint"
    mountpoint -q "$mountpoint" || sudo mount -t overlay overlay -o lowerdir="$base/root.x86_64",upperdir="$overlay/upper",workdir="$overlay/work" "$mountpoint"
}

download_and_unpack_image "$version" "$base"
mount_image "$mountpoint" "$base" "$internal/overlays/$instance"

[ -f "$mountpoint/etc/fyl-arch-chroot-initialization.ok" ] || sudo tee "$mountpoint/etc/profile.d/00-fyl-arch-chroot-initialization.sh" <<"EOF" >/dev/null

# set locale
echo en_US.UTF-8 UTF-8 >/etc/locale.gen
locale-gen
echo LANG=en_US.UTF-8 >/etc/locale.conf

# initialize Pacman
echo 'Server = https://mirror.rackspace.com/archlinux/$repo/os/$arch' >/etc/pacman.d/mirrorlist
pacman-key --init
pacman-key --populate archlinux
pacman -Syu --needed --noconfirm base-devel

touch /etc/fyl-arch-chroot-initialization.ok
rm /etc/profile.d/00-fyl-arch-chroot-initialization.sh
EOF

exec sudo "$mountpoint/bin/arch-chroot" "$mountpoint" /bin/bash -l
