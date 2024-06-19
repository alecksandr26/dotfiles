
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
    fdisk /dev/sda <<EOF
n
p
1

+512M
t
1
uefi
n
p
2

+16G
t
2
swap
n
p
3


t
3
linux
w
EOF
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




