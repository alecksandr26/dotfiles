#!/usr/bin/env bash
set -euo pipefail

# To connect ssh: ssh -o UserKnownHostsFile=/dev/null root@192.168.100.66
# The whole script is based on the Installation Guide
# https://wiki.archlinux.org/title/Installation_guide

LOG_FILE="/tmp/arch_install.log"
exec > >(tee -a "$LOG_FILE") 2>&1
echo "Logging to $LOG_FILE"

# Helper: get partition path (handles NVMe /dev/nvme0n1p1 vs /dev/sda1)
part() {
    local disk=$1 num=$2
    if [[ "$disk" == *nvme* ]] || [[ "$disk" == *mmcblk* ]]; then
        echo "${disk}p${num}"
    else
        echo "${disk}${num}"
    fi
}

# Function to check internet connection
check_internet() {
    echo "Checking internet connection..."
    ping -c 1 archlinux.org &> /dev/null
    echo "Internet connection is active."
}

partition_disk() {
    local disk=$1
    echo "Partitioning the disk..."
    sleep 1
    read -p "Put the size of efi partition (e.g., 512M): " efi_size
    read -p "Put the size of swap partition (e.g., 4G): " swap_size
    (
    echo g # Create a new GPT partition table
    echo n # Add a new partition
    echo 1 # Partition number
    echo   # First sector (Accept default: 1)
    echo "+$efi_size" # Last sector
    echo t # Change partition type
    echo 1 # Select partition 1

    echo n # Add a new partition
    echo 2 # Partition number
    echo   # First sector (Accept default: next free)
    echo "+$swap_size" # Last sector
    echo t # Change partition type
    echo 2 # Select partition 2
    echo 19 # Type Linux swap

    echo n # Add a new partition
    echo 3 # Partition number
    echo   # First sector (Accept default: next free)
    echo   # Last sector (Accept default: varies)
    echo t # Change partition type
    echo 3 # Select partition 3
    echo 20 # Type Linux filesystem

    echo w # Write changes
    ) | fdisk "$disk"

    echo "Disk partitioning completed successfully."
}

formatting() {
    local disk=$1
    echo "Formatting $(part "$disk" 1) as FAT32 (EFI)..."
    mkfs.fat -F32 "$(part "$disk" 1)"

    echo "Creating swap on $(part "$disk" 2)..."
    mkswap "$(part "$disk" 2)"

    echo "Formatting $(part "$disk" 3) as ext4..."
    mkfs.ext4 -F "$(part "$disk" 3)"
}

mounting() {
    local disk=$1
    echo "Mounting $(part "$disk" 3) to /mnt..."
    mount "$(part "$disk" 3)" /mnt

    echo "Mounting $(part "$disk" 1) to /mnt/boot..."
    mkdir -p /mnt/boot
    mount "$(part "$disk" 1)" /mnt/boot

    echo "Enabling swap on $(part "$disk" 2)..."
    swapon "$(part "$disk" 2)"
}

# 1. Check Internet Connection
check_internet

# 2. Updating the system clock
echo "Updating the system clock..."
timedatectl set-ntp true
timedatectl status

# 3. Partition the Disk
# NOTE: Partition scheme is EFI + swap + root (no separate /home)
echo "Checking the disks..."
fdisk -l
disk_device=""
while [ -z "$disk_device" ]; do
    read -p "Enter the disk device (e.g., /dev/sda or /dev/nvme0n1): " disk_device
    if [ -z "$disk_device" ]; then
        echo "Disk device cannot be empty. Please try again."
    elif [ ! -b "$disk_device" ]; then
        echo "'$disk_device' is not a valid block device. Please try again."
        disk_device=""
    fi
done
partition_disk "$disk_device"

# 4. Format and Mount Partitions
echo "Formatting and mounting partitions..."
formatting "$disk_device"
mounting "$disk_device"

# 5. Install Base System
echo "Installing base system..."
pacstrap /mnt base linux linux-firmware linux-firmware-amdgpu

# 6. Generate fstab
echo "Generating fstab..."
genfstab -U /mnt >> /mnt/etc/fstab

# 7. Chroot into the New System
echo "Chrooting into the new system and configure it..."

read -r -p "Put the hostname: " hostname
while true; do
    read -s -r -p "Put the root's passwd: " root_passwd
    echo ""
    read -s -r -p "Re-enter the root's passwd: " root_passwd_confirm
    echo ""
    if [ "$root_passwd" != "$root_passwd_confirm" ]; then
        echo "Passwords do not match. Please try again."
    else
        echo "Passwords match."
        break
    fi
done
read -r -p "Put an username: " username
while true; do
    read -s -r -p "Put user's passwd: " user_passwd
    echo ""
    read -s -r -p "Re-enter user's passwd: " user_passwd_confirm
    echo ""
    if [ "$user_passwd" != "$user_passwd_confirm" ]; then
        echo "Passwords do not match. Please try again."
    else
        echo "Passwords match."
        break
    fi
done

echo "Available timezones (examples: America/Chicago, Europe/London, Mexico/General):"
ls /usr/share/zoneinfo/ | head -20
echo "..."
read -r -p "Enter your timezone (e.g., Mexico/General): " timezone
while [ ! -f "/usr/share/zoneinfo/$timezone" ]; do
    echo "'$timezone' is not a valid timezone."
    read -r -p "Enter your timezone (e.g., Mexico/General): " timezone
done

export DISK_DEVICE=$disk_device
export HOSTNAME=$hostname
export USERNAME=$username
export ROOT_PASSWD=$root_passwd
export USER_PASSWD=$user_passwd
export TIMEZONE=$timezone

arch-chroot /mnt /bin/bash <<EOF
set -euo pipefail

# 8. Configure the System
echo "Setting timezone to \$TIMEZONE..."
ln -sf /usr/share/zoneinfo/\$TIMEZONE /etc/localtime
hwclock --systohc

echo "Configuring locale..."
grep -qxF "en_US.UTF-8 UTF-8" /etc/locale.gen || echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
grep -qxF "es_MX.UTF-8 UTF-8" /etc/locale.gen || echo "es_MX.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf

echo "Configuring vconsole..."
echo "KEYMAP=us" > /etc/vconsole.conf

echo "Setting hostname..."
echo "\$HOSTNAME" > /etc/hostname
cat > /etc/hosts <<HOSTS
127.0.0.1	localhost
::1		localhost
127.0.1.1	\$HOSTNAME.localdomain	\$HOSTNAME
HOSTS

# 9. Configure Network
echo "Enabling network service..."
pacman -S --noconfirm networkmanager dhcpcd
systemctl enable NetworkManager
systemctl enable dhcpcd

# 10. Install GRUB Boot Loader and Configure it
echo "Downloading GRUB boot loader..."
pacman -S --noconfirm grub efibootmgr
echo "Installing GRUB boot loader..."
grub-install --target=x86_64-efi --efi-directory=/boot/ --bootloader-id=GRUB
echo "Configuring GRUB..."
grub-mkconfig -o /boot/grub/grub.cfg

# 10.1 Add resume hook for hibernation and regenerate initramfs
echo "Adding resume hook to mkinitcpio..."
sed -i 's/^HOOKS=(\(.*\)filesystems\(.*\))/HOOKS=(\1filesystems resume\2)/' /etc/mkinitcpio.conf
mkinitcpio -P

# 11. Set Root Password and Create User
echo "Setting root password..."
chpasswd <<< "root:\$ROOT_PASSWD"

echo "Creating user \$USERNAME..."
useradd -m -G wheel,sys,rfkill,input "\$USERNAME"

echo "Setting up XDG user directories..."
pacman -S --noconfirm xdg-user-dirs
su - "\$USERNAME" -c "xdg-user-dirs-update"

echo "Setting the password for user \$USERNAME..."
chpasswd <<< "\$USERNAME:\$USER_PASSWD"

# 12. Set sudo
echo "Installing and configuring sudo..."
pacman -S --noconfirm sudo
sed -i 's/# %wheel ALL=(ALL:ALL) ALL/%wheel ALL=(ALL:ALL) ALL/' /etc/sudoers

EOF

echo "Chroot configuration completed successfully."

# 13. Exit archiso and Reboot
echo "Exiting chroot, unmounting and rebooting..."
echo "Unmounting recursive /mnt/..."
umount -R /mnt

echo "Swapoff $(part "$disk_device" 2)"
swapoff "$(part "$disk_device" 2)"

read -n1 -r -p "Press any key to reboot the system..." key
echo ""
echo "Rebooting..."
sleep 2
reboot
