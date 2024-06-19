# To connect ssh: ssh -o UserKnownHostsFile=/dev/null root@192.168.100.66
# The whole script is base on the Installation Guide
# https://wiki.archlinux.org/title/Installation_guide

# Function to check internet connection
check_internet() {
    echo "Checking internet connection..."
    ping -c 1 archlinux.org &> /dev/null
    if [ $? -ne 0 ]; then
        echo "No internet connection detected. Please check your connection and try again."
        exit 1
    else
        echo "Internet connection is active."
    fi
}



partition_disk() {
    echo "Partitioning the disk..."
(
    echo n # Add a new partition
    echo p 
    echo 1 # Partition number
    echo   # First sector (Accept default: 1)
    echo +512M # Last sector
    echo t # Change partition type
    echo 1 # Select partition 1
    # echo 1 # Type EFI System

    echo n # Add a new partition
    echo p
    echo 2 # Partition number
    echo   # First sector (Accept default: next free)
    echo +4G # Last sector
    echo t # Change partition type
    echo 2 # Select partition 2
    echo 19 # Type Linux swap

    echo n # Add a new partition
    echo p
    echo 3 # Partition number
    echo   # First sector (Accept default: next free)
    echo   # Last sector (Accept default: varies)
    echo t # Change partition type
    echo 3 # Select partition 3
    echo 20 # Type Linux filesystem

    echo w # Write changes
) | fdisk /dev/sda

# Check if fdisk commands executed successfully
if [ $? -ne 0 ]; then
    echo "Partitioning failed. Please check the fdisk commands and try again."
    exit 1
else
    echo "The partition was successful."
fi
}

formatting() {
    mkfs.fat -F32 /dev/sda1
    mkswap /dev/sda2
    (echo y) | mkfs.ext4 /dev/sda3
}

mounting() {
    mount /dev/sda3 /mnt
    mkdir /mnt/boot
    mount /dev/sda1 /mnt/boot
    swapon /dev/sda2
}

# 1. Check Internet Connection
check_internet

# 2. Updating the system clock
echo "Updating the system clock..."
timedatectl set-ntp true
echo "Checking the service..."
timedatectl status

# 2. Partition the Disk
echo "Checking the disks..."
fdisk -l
partition_disk


# 3. Format and Mount Partitions
echo "Formatting and mounting partitions..."
formatting
mounting

# 4. Install Base System
echo "Installing base system..."
pacstrap /mnt base linux linux-firmware


# 5. Generate fstab
echo "Generating fstab..."
genfstab -U /mnt >> /mnt/etc/fstab


# 6. Chroot into the New System
echo "Chrooting into the new system..."
arch-chroot /mnt /bin/bash <<EOF_CHROOT

configure_system() {
    echo "Setting timezone..."
    ln -sf /usr/share/zoneinfo/Mexico/General /etc/localtime
    hwclock --systohc

    echo "Configuring locale..."
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
    echo "es_MX.UTF-8 UTF-8" >> /etc/locale.gen
    locale-gen
    echo "LANG=en_US.UTF-8" > /etc/locale.conf

    echo "Configuring vconsole..."
    echo "KEYMAP=us" > /etc/vconsole.conf

    echo "Setting hostname..."
    echo "arch" > /etc/hostname
    echo -e "127.0.0.1 \t localhost \n ::1 \t \t localhost \n 127.0.1.1 \t arch.localdomain \t arch" >> /etc/hosts 
}


configure_network() {
    echo "Enabling network service..."
    (echo y) | pacman -S networkmanager dhcpcd
    systemctl enable NetworkManager
    systemctl enable dhcpcd	    		    		    		   
}

installing_grub() {
    echo "Installing GRUB boot loader..."
    (echo y) | pacman -S grub efibootmgr
    grub-install --target=x86_64-efi --efi-directory=/boot/ --bootloader-id=GRUB
    echo "Configuring GRUB..."
    grub-mkconfig -o /boot/grub/grub.cfg
}

configure_new_user() {
    echo "Setting root password..."
    passwd

    echo "Creating user and the sudo..."
    echo -e "Put your username: "
    read username
    useradd -m -G wheel $username
    echo "Setting the password for user $username..."
    passwd $username	
}


configure_sudo() {
    echo "Installing sudo..."
    (echo y) | pacman -S sudo vim
    echo "Configuring sudo..."
    sleep 2
    EDITOR=vim visudo	
}


# 7. Configure the System
configure_system


# 8. Configure Network
configure_network

# 9. Install GRUB Boot Loader and Configure it
installing_grub


# 10. Set Root Password and Create User
configure_new_user


# 11. Set sudo
configure_sudo


EOF_CHROOT


# 12. Exit archiso and Reboot
echo "Exiting chroot and rebooting..."
umount -R /mnt
reboot


