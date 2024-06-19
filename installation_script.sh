
# 
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
    mkfs.ext4 /dev/sda3    
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
arch-chroot /mnt



