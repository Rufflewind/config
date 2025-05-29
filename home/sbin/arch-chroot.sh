#!/bin/bash
set -eux

# The script is written based on the upstream documentation:
# https://wiki.archlinux.org/title/Install_Arch_Linux_from_existing_Linux#Method_A:_Using_the_bootstrap_image_(recommended)
#
# Things to do after creating an instance:
# (a) Set up a wheel account with NOPASSWD:
#
#     # echo '%wheel ALL=(ALL:ALL) NOPASSWD: ALL' > /etc/sudoers.d/00_wheel_nopasswd
#     # useradd -m -G wheel <name>
#     # su -l <name>
#
# (b) Use install-yay.sh to bootstrap yay:
# https://raw.githubusercontent.com/Rufflewind/config/master/install-yay.sh
#
# ------------------------------------------------------------------------
#
# Find the latest version here: https://mirror.rackspace.com/archlinux/iso/
# Note that this version only affects new instances.
version=2025.05.01
internal=/opt/arch-chroot
mountbase=/mnt/arch

[ "${1+x}" = x ] || {
    cat >&2 <<EOF
Usage: $(basename "$0") <instance>

where <instance> is an arbitrary name for the instance.

This script will create an instance if it doesn't already exist.
New instances will default to on version $version.
To select a different version, edit this script.

Data is stored in $internal while mounts are added to $mountbase.
To delete an instance, run:

    sudo umount /mnt/arch/\$instance  # if needed
    sudo rmdir /mnt/arch/\$instance
    sudo rm -R --one-file-system /opt/arch-chroot/overlays/\$instance
EOF
    exit 2
}
instance=$1

mountpoint=$mountbase/$instance       # overlay fs mountpoint
base=$internal/images/$version        # image data
overlay=$internal/overlays/$instance  # overlay fs data

# ------------------------------------------------------------------------

download_file() {
    local out=$1
    local url=$2
    if [ -f "$out" ]; then
        return
    fi
    sudo mkdir -p "$(dirname "$out")"
    curl -LSfs "$url" | sudo tee "$out.tmp" >/dev/null
    sudo mv "$out.tmp" "$out"
}

download_and_unpack_image() {
    local version=$1
    local base=$2
    local tar=$base/archlinux-bootstrap-$version-x86_64.tar.zst
    local out=$base/.ok
    if [ -f "$out" ]; then
        return
    fi
    download_file "$tar" "https://mirror.rackspace.com/archlinux/iso/$version/archlinux-bootstrap-$version-x86_64.tar.zst"
    download_file "$tar.sig" "https://mirror.rackspace.com/archlinux/iso/$version/archlinux-bootstrap-$version-x86_64.tar.zst.sig"
    sudo GNUPGHOME=$base/.gnupg gpg --keyserver-options auto-key-retrieve --verify "$tar.sig"
    sudo tar --zstd -x -C "$base" -f "$tar"
    sudo touch "$out"
}

mount_image() {
    local overlay=$1
    local mountpoint=$2
    sudo mkdir -p "$overlay/upper" "$overlay/work" "$mountpoint"
    mountpoint -q "$mountpoint" || sudo mount -t overlay overlay -o lowerdir="$overlay/lower",upperdir="$overlay/upper",workdir="$overlay/work" "$mountpoint"
}

# Attach image if the lower dir isn't yet set up.  But if the lower dir
# already exists, keep using the same image for consistency.
[ -L "$overlay/lower" ] || {
    download_and_unpack_image "$version" "$base"
    sudo mkdir -p "$overlay"
    sudo ln -Trs "$base/root.x86_64" "$overlay/lower"
}
mount_image "$overlay" "$mountpoint"

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
