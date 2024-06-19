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
(
    echo g # Create a new GPT partition table
    echo n # Add a new partition
    echo 1 # Partition number
    echo   # First sector (Accept default: 1)
    echo +512M # Last sector
    echo t # Change partition type
    echo 1 # Select partition 1
    # echo 1 # Type EFI System

    echo n # Add a new partition
    echo 2 # Partition number
    echo   # First sector (Accept default: next free)
    echo +4G # Last sector
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


read -r -p "Put the root's passwd: " root_passwd
read -r -p "Put an username: " username
read -r -p "Put username's passwd: " username_passwd

# Export variables to be available in arch-chroot
export DISK_DEVICE=$disk_device

export USERNAME="$username"
export ROOTPASSWD="$root_passwd"
export USERNAMEPASSWD="$username_passwd"

echo "USERNAME: $USERNAME"
echo "USERNAMEPASSWD: $USERNAMEPASSWD"
echo "ROOTPASSWD: $ROOTPASSWD"

sleep 1

arch-chroot /mnt /bin/bash <<EOF_CHROOT

echo $USERNAME
echo $USERNAMEPASSWD
echo $ROOTPASSWD

# Use the exported variables
root_passwd="\$ROOTPASSWD"
username="\$USERNAME"
username_passwd="\$USERNAMEPASSWD"

echo $username
echo $root_passwd
echo $username_passwd

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
    echo "arch" > /etc/hostname
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
    # (echo ${root_passwd}) | passwd
    passwd

    echo "Creating user and the sudo..."
    sleep 2
    useradd -m -G wheel $username
    echo "Setting the password for user ${username}..."
    sleep 2
    # (echo ${username_passwd}) | passwd $username
    passwd $username
}


configure_sudo() {
    echo "Installing sudo..."
    sleep 2
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
# configure_sudo


EOF_CHROOT

if [ $? -ne 0 ]; then
    echo "Chrooting failed."
    exit 1
else
    echo "Chrooting success."
fi


# 12. Exit archiso and Reboot
echo "Exiting chroot and rebooting..."
read -n1 -r -p "Press any key to continue..." key
echo "Umounting /mnt/..."
sleep 1
umount -R /mnt
if [ $? -ne 0 ]; then
    echo "Umounting /mnt/ failed."
    exit 1
else
    echo "Umounting /mnt/ success."
fi

echo "Rebooting..."
sleep 2
reboot


