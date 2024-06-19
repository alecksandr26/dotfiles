
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
echo n # New partition
echo 1 # Partition number
echo   # First sector (Accept default: 1)
echo +512M # Last sector (Accept default: varies)
echo t # Change partition type
echo 1 # Partition number
echo 1 # EFI System

echo n # New partition
echo 2 # Partition number
echo   # First sector (Accept default: next free)
echo +16G # Last sector
echo t # Change partition type
echo 2 # Partition number
echo 19 # Linux swap

echo n # New partition
echo 3 # Partition number
echo   # First sector (Accept default: next free)
echo   # Last sector (Accept default: varies)
echo t # Change partition type
echo 3 # Partition number
echo 20 # Linux filesystem

echo w # Write changes
) | fdisk /dev/sda
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




