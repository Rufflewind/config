# Source:         Ruflewind/config:root/boot/install-efiboot-arch.sh
# Documentation:  https://wiki.archlinux.org/title/EFISTUB#Booting_EFISTUB
# Dependencies:   sudo pacman -S --needed efibootmgr amd-ucode intel-ucode

label=...                      # label for boot entry
esp_disk=/dev/disk/by-id/...   # disk containing the EFI system partition
esp_part=...                   # EFI system partition number (1-indexed)
root=PARTUUID=...              # UUID of the root (/) partition

case `efibootmgr` in
    *"$label"*)
        printf >&2 "skipping because %s already exists\n" "$label"
        exit 2
        ;;
    *) ;;
esac
efibootmgr \
  --create \
  --unicode \
  --verbose \
  --disk "$esp_disk" \
  --part "$esp_part" \
  --label "$label" \
  --loader '\vmlinuz-linux' \
  'rw root='"$root"' initrd=\amd-ucode.img initrd=\intel-ucode.img initrd=\initramfs-linux.img'
