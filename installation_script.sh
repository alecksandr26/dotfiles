#!/usr/bin/env bash

# To connect ssh: ssh -o UserKnownHostsFile=/dev/null root@192.168.100.66
# The whole script is base on the Installation Guide
# https://wiki.archlinux.org/title/Installation_guide

# Function to check internet connection
check_internet() {
    echo "Checking internet connection..."
    sleep 2
    ping -c 1 archlinux.org &> /dev/null
    if [ $? -ne 0 ]; then
        echo "No internet connection detected. Please check your connection and try again."
        exit 1
    else
        echo "Internet connection is active."
    fi
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
    # echo 1 # Type EFI System

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
    ) | fdisk $disk

    # Check if fdisk commands executed successfully
    if [ $? -ne 0 ]; then
	echo "Partitioning failed. Please check the fdisk commands and try again."
	exit 1
    else
	echo "Disk partitioning completed successfully."
    fi
}

formatting() {
    local disk=$1
    mkfs.fat -F32 ${disk}1
    if [ $? -ne 0 ]; then
	echo "Formating /dev/sda1 failed."
	exit 1
    else
	echo "Formating /dev/sda1 success."
    fi
    
    mkswap ${disk}2
    if [ $? -ne 0 ]; then
	echo "Making swapon /dev/sda2 failed."
	exit 1
    else
	echo "Making swapon /dev/sda2 success."
    fi
    
    
    (echo y) | mkfs.ext4 ${disk}3
    if [ $? -ne 0 ]; then
	echo "Formating /dev/sda3 failed."
	exit 1
    else
	echo "Formating /dev/sda3 success."
    fi
}

mounting() {
    local disk=$1
    mount ${disk}3 /mnt
    if [ $? -ne 0 ]; then
	echo "Mounting /dev/sda3 to /mnt failed."
	exit 1
    else
	echo "Mounting /dev/sda3 to /mnt success."
    fi
    mkdir /mnt/boot
    mount ${disk}1 /mnt/boot
    if [ $? -ne 0 ]; then
	echo "Mounting /dev/sda1 to /mnt/boot failed."
	exit 1
    else
	echo "Mounting /dev/sda1 to /mnt/boot success."
    fi
    swapon ${disk}2
    if [ $? -ne 0 ]; then
	echo "Swaponing /dev/sda2 failed."
	exit 1
    else
	echo "Swaponing /dev/sda2 success."
    fi
}

# 1. Check Internet Connection
check_internet

# 2. Updating the system clock
echo "Updating the system clock..."
sleep 2
timedatectl set-ntp true

echo "Checking the clock service..."
sleep 2
timedatectl status

# 2. Partition the Disk
echo "Checking the disks..."
sleep 2
fdisk -l
read -p "Enter the disk device (e.g., /dev/sda): " disk_device
partition_disk $disk_device


# 3. Format and Mount Partitions
echo "Formatting and mounting partitions..."
sleep 2
formatting $disk_device
sleep 2
mounting $disk_device

# 4. Install Base System
echo "Installing base system..."
sleep 2
pacstrap /mnt base linux linux-firmware
if [ $? -ne 0 ]; then
    echo "Base system installation failed."
    exit 1
else
    echo "Base system installation success."
fi


# 5. Generate fstab
echo "Generating fstab..."
sleep 2
genfstab -U /mnt >> /mnt/etc/fstab
if [ $? -ne 0 ]; then
    echo "Generating fstab failed."
    exit 1
else
    echo "Generating fstab success."
fi

# 6. Chroot into the New System
echo "Chrooting into the new system and configure it..."
sleep 2


read -r -p "Put the hostname: " hostname
while true; do
    # Prompt user to enter root password
    read -s -r -p "Put the root's passwd: " root_passwd
    echo ""

    # Prompt user to re-enter root password
    read -s -r -p "Re-enter the root's passwd: " root_passwd_confirm
    echo ""

    # Check if passwords match
    if [ "$root_passwd" != "$root_passwd_confirm" ]; then
        echo "Passwords do not match. Please try again."
    else
        echo "Passwords match."
        break  # Exit the loop if passwords match
    fi
done
read -r -p "Put an username: " username
while true; do
    # Prompt user to enter user's password
    read -s -r -p "Put user's passwd: " user_passwd
    echo ""

    # Prompt user to re-enter user's password
    read -s -r -p "Re-enter user's passwd: " user_passwd_confirm
    echo ""

    # Check if passwords match
    if [ "$user_passwd" != "$user_passwd_confirm" ]; then
        echo "Passwords do not match. Please try again."
    else
        echo "Passwords match."
        break  # Exit the loop if passwords match
    fi
done

# Export variables to be available in arch-chroot
# The issue you're encountering stems from how variables are expanded within a here document (<<EOF). Specifically, variables inside a here document are subject to expansion at the time the here document is parsed, not when it's executed. This can lead to unexpected behavior, especially with special characters like # and $.

# Problem Explanation
# When you use a here document (<<EOF) in Bash, variables inside the here document are expanded before the script inside the here document is executed. This means any special characters or variable syntax ($) inside the variable PASSWD will be interpreted prematurely, potentially causing syntax errors or unexpected behavior.

# Solution: Quoting and Here Document
# To solve this problem, you need to ensure that variables inside the here document are quoted properly to delay their expansion until runtime, when they are actually used within the here document script.

# Here’s how you can modify your script to handle special characters correctly:
export DISK_DEVICE=$disk_device
export HOSTNAME=$hostname
export USERNAME=$username
export ROOT_PASSWD=$root_passwd
export USER_PASSWD=$user_passwd

sleep 1

arch-chroot /mnt /bin/bash <<EOF
#!/usr/bin/env bash

configure_system() {
    echo "Setting timezone..."
    sleep 2
    ln -sf /usr/share/zoneinfo/Mexico/General /etc/localtime
    hwclock --systohc

    echo "Configuring locale..."
    sleep 2
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
    echo "es_MX.UTF-8 UTF-8" >> /etc/locale.gen
    locale-gen
    echo "LANG=en_US.UTF-8" > /etc/locale.conf

    echo "Configuring vconsole..."
    sleep 2
    echo "KEYMAP=us" > /etc/vconsole.conf

    echo "Setting hostname..."
    sleep 2
    echo "\$HOSTNAME" > /etc/hostname
    echo -e "127.0.0.1 \t localhost \n ::1 \t \t localhost \n 127.0.1.1 \t arch.localdomain \t arch" >> /etc/hosts 
}


configure_network() {
    echo "Enabling network service..."
    sleep 2
    (echo y) | pacman -S networkmanager dhcpcd
    systemctl enable NetworkManager
    systemctl enable dhcpcd
}

installing_grub() {
    echo "Installing GRUB boot loader..."
    sleep 2
    (echo y) | pacman -S grub efibootmgr
    grub-install --target=x86_64-efi --efi-directory=/boot/ --bootloader-id=GRUB
    echo "Configuring GRUB..."
    grub-mkconfig -o /boot/grub/grub.cfg
}

configure_new_user() {
    echo "Setting root password..."
    sleep 2
    (echo root:"\$ROOT_PASSWD") | chpasswd

    echo "Creating user "\$USERNAME"..."
    sleep 2
    useradd -m -G wheel,sys,rfkill,input  "\$USERNAME"
    echo "Setting the password for user "\$USERNAME"..."
    sleep 2
    (echo "\$USERNAME":"\$USER_PASSWD") | chpasswd
}


configure_sudo() {
    echo "Installing sudo..."
    sleep 2
    (echo y) | pacman -S sudo
    echo "Configuring sudo..."
    sleep 2
    sed -i 's/# %wheel ALL=(ALL:ALL) ALL/%wheel ALL=(ALL:ALL) ALL/' /etc/sudoers
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
# configure_sudo


EOF

if [ $? -ne 0 ]; then
    echo "Chrooting failed."
    exit 1
else
    echo "Chrooting success."
fi


# 12. Exit archiso and Reboot
echo "Exiting chroot, umounting and rebooting..."
sleep 2
echo "Umounting recursive /mnt/..."
sleep 1
umount -R /mnt
if [ $? -ne 0 ]; then
    echo "Umounting /mnt/ failed."
    exit 1
else
    echo "Umounting /mnt/ success."
fi

echo "swappingoff /dev/sda2"
sleep 1
swapoff /dev/sda2


read -n1 -r -p "Press any key to reboot the system..." key
echo "Rebooting..."
sleep 2
reboot


