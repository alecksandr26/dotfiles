#!/usr/bin/env bash
set -euo pipefail

LOG_FILE="$HOME/software_install.log"
exec > >(tee -a "$LOG_FILE") 2>&1
echo "Logging to $LOG_FILE"

SCRIPT_PATH="$(realpath "$0")"

# 1. Installing Yay
echo "Installing Yay..."
sudo pacman -S --noconfirm git base-devel cmake
cd ~/Downloads || mkdir -p ~/Downloads && cd ~/Downloads
if [ ! -d yay ]; then
    git clone https://aur.archlinux.org/yay.git
fi
cd yay/
makepkg -si --noconfirm

# 2. Downloading my dotfiles
echo "Downloading the dotfiles..."
cd ~/Documents || mkdir -p ~/Documents && cd ~/Documents
if [ ! -d dotfiles ]; then
    git clone https://github.com/alecksandr26/dotfiles
fi
cd dotfiles/

# 3. Installing xorg and xfce
echo "Installing xorg and xfce..."
sudo pacman -S --noconfirm xorg xorg-xinit xfce4 xfce4-goodies xf86-video-amdgpu


# 3.1. Installing drivers (RX 5500 XT / Navi 14)
echo "Installing drivers..."
sudo pacman -S --noconfirm mesa vulkan-radeon libva-utils


# 3.2 Enabling the multilib
echo "Enabling the multilib..."
sudo sed -i "/\[multilib\]/,/Include/"'s/^#//' /etc/pacman.conf
sudo pacman -Sy
sudo pacman -S --noconfirm lib32-mesa lib32-vulkan-radeon


# 3.3 Installing and enabling a Display Manager (to avoid TTY)
echo "Installing LightDM..."
sudo pacman -S --noconfirm lightdm lightdm-gtk-greeter arandr
sudo systemctl enable lightdm

echo "Remember running `arandr` and creating the display `display-setup.sh` and adding this `xrdb -merge /home/aleck/.Xresources` "

# 4. Configure xorg and xfce
echo "Configuring xorg and xfce..."
cp .xinitrc ~ # for starting with xstart
cp .Xresources ~
mkdir -p ~/.config/
sudo mkdir -p /usr/share/themes/empty/xfwm4/
sudo touch /usr/share/themes/empty/xfwm4/themerc

# Setting up the config for tear free
sudo cp ~/Documents/dotfiles/20-amdgpu.conf /etc/X11/xorg.conf.d/

# 5. PipeWire audio server and firmware
echo "Installing PipeWire..."
sudo pacman -S --noconfirm sof-firmware alsa-firmware pipewire pipewire-alsa pipewire-pulse wireplumber

# 5.1 Bluetooth audio and things
echo "Installing Blueman for bluetooth..."
sudo pacman -S --noconfirm bluez bluez-utils blueman
sudo systemctl enable bluetooth.service

# 6. Installing terminal emulator stuff
echo "Installing terminal emulator stuff..."
sudo pacman -S --noconfirm \
     kitty \
     tmux nano vim \
     nnn \
     less \
     htop \
     zip unzip xdg-utils tree adobe-source-code-pro-fonts

yay -S --noconfirm timg moc-pulse

# Usage for MOC
# Launch the player
# mocp

# Basic Controls (Inside UI):
# ENTER      : Play track / Enter directory
# TAB        : Switch between file browser (left) and playlist (right)
# a          : Add track/folder to playlist
# s          : Stop
# n / b      : Next / Previous track
# < / >      : Volume down / up
# q          : Detach (Close UI, music keeps playing)
# Q          : Quit (Kills the server and the music)

# # CLI Flags:
# mocp -i    : Show current song info
# mocp -x    : Kill the background server


# Usage for timg
# View a single image or GIF
# timg image.png

# View a video file in the terminal
# timg video.mp4

# Display images in a grid
# timg --grid 3x2 /path/to/folder/

# Check terminal capability (Sixel/Kitty support)
# timg --info

# Guides for the weird software:
# nnn: https://opensource.com/article/22/12/linux-file-manager-nnn
# nnn: Usage https://github.com/jarun/nnn/wiki/Usage#keyboard-mouse
# NAVIGATION
#         Up k  Up                PgUp ^U  Page up
#         Dn j  Down              PgDn ^D  Page down
#         Lt h  Parent            ~ ` @ -  ~, /, start, prev
#     Ret Rt l  Open                    '  First file/match
#         g ^A  Top                     J  Jump to entry/offset
#         G ^E  End                    ^J  Toggle auto-advance on open
#        B (,)  Book(mark)           b ^/  Select bookmark
#          1-4  Context           (Sh)Tab  Cycle/new context
#      2Esc ^Q  Quit                   ^y  Next young
#           ^G  QuitCD                  Q  Pick/err, quit
#    q Alt+Esc  Quit context           ^L  Refresh dir

# FILTER & PROMPT
#            /  Filter                 ^N  Toggle type-to-nav
#          Esc  Exit prompt            ^L  Toggle last filter
#            .  Toggle hidden

# FILES
#         o ^O  Open with...            n  Create new/link
#         f ^F  File details            d  Detail mode toggle
#           ^R  Rename/dup              r  Batch rename
#            z  Archive                 e  Edit file
#            *  Toggle exe              >  Export list
#      Space +  (Un)select            m-m  Select range/clear
#            a  Select all              A  Invert sel
#         p ^P  Copy here            w ^W  Cp/mv sel as
#         v ^V  Move here               E  Edit sel list
#         x ^X  Delete or trash         S  Listed sel size
#            X  Delete (rm -rf)       Esc  Send to FIFO

# MISC
#        Alt ;  Select plugin           =  Launch app
#         ! ^]  Shell                   ]  Cmd prompt
#            c  Connect remote          u  Unmount remote/archive
#         t ^T  Sort toggles            s  Manage session
#            T  Set time type           0  Lock
#           ^L  Redraw                  ?  Help, conf

# 6.1 Configure the terminal emulator stuff
echo "Configuring terminal emulator stuff..."
cp .bashrc ~/
mkdir -p ~/.config/kitty
cp kitty.conf ~/.config/kitty/
cp .nanorc ~/
cp .vimrc ~/
cp .tmux.conf ~/

# 7. Installing basic graphics tools and utilities
echo "Installing graphics tools, utilities, recording, editing, and virtualization software..."
sudo pacman -S --noconfirm \
    firefox thunar file-roller gvfs tumbler ffmpegthumbnailer pavucontrol libdvdcss vlc feh mypaint yt-dlp \
    xclip maim \
    obs-studio audacity \
    kdenlive gimp \
    qemu-full virt-manager libvirt dnsmasq iptables-nft dmidecode swtpm


# 7.1 Enabling  this things
sudo systemctl start libvirtd.service
sudo systemctl enable libvirtd.service

: <<'END_COMMENT'
# Basic Guide to Using QEMU/KVM + libvirt + virt‑manager on Arch Linux

This document explains how to manage NAT networks and create virtual machines using QEMU/KVM with `libvirt`, `virsh`, and `virt‑manager` on Arch Linux. It can be used as both **internal documentation** and as a **commented configuration guide**.

---

## 1. List and manage NAT networks

### 1.1 List all virtual networks

Libvirt allows you to create virtual networks (NAT, bridge, isolated, etc.). To list all networks:

```bash
virsh net-list --all
```

Example output:
Name State Autostart

default active yes
red_nat_lab active no

- `Name`: network name (used in `virsh net-start <name>` and in `virt‑manager`).  
- `State`: `active` (running) or `inactive`.  
- `Autostart`: `yes` if the network starts automatically when the host boots.

### 1.2 Inspect network details (IP, DHCP, bridge)

To see the configuration of a network (for example, `default`):

```bash
virsh net-dumpxml default
```

Typical fragment of the output:

```xml
<bridge name='virbr0' stp='on' delay='0'/>
<ip address='192.168.122.1' netmask='255.255.255.0'>
  <dhcp>
    <range start='192.168.122.100' end='192.168.122.200'/>
  </dhcp>
</ip>
```

- `virbr0`: virtual bridge created by libvirt.  
- `192.168.122.1`: default gateway / NAT gateway of the network.  
- DHCP range: `192.168.122.100–192.168.122.200` (assigned to VMs automatically).

### 1.3 Start and enable autostart of a NAT network

For the `default` NAT network:

```bash
virsh net-start default
virsh net-autostart default
```

- `net-start`: starts the given virtual network.  
- `net-autostart`: configures the network to start automatically when the host boots up.

---

## 2. Create a virtual machine with QEMU/KVM

### 2.1 Install required packages on Arch Linux

Make sure the following packages are installed:

```bash
sudo pacman -S qemu-full libvirt virt-manager dnsmasq iptables-nft
```

- `qemu-full`: full QEMU emulator with support for all architectures.  
- `libvirt`: daemon and API that manages VMs, networks, and storage.  
- `virt-manager`: graphical UI for creating and managing VMs (similar to VirtualBox).  
- `dnsmasq`: provides DHCP/DNS for NAT networks.  
- `iptables-nft`: handles NAT firewall rules for the virtual networks.

### 2.2 Enable and start the libvirt service

```bash
sudo systemctl enable --now libvirtd.service
```

Verify that the service is running:

```bash
sudo systemctl status libvirtd.service
```

You should see `active (running)`.  
- If your user is not yet in the `libvirt` group, add it:

```bash
sudo usermod -aG libvirt $USER
```

Then **log out and log back in** so the group membership takes effect in your session.

---

## 3. Create a virtual machine with virt‑manager (GUI)

1. Open `virt-manager`:

```bash
virt-manager
```

2. In the main window:
   - Click **File → New Virtual Machine**.

3. Choose installation type:
   - **Local install media (ISO image or CDROM)**  
     or  
   - **Import existing disk image**.

4. Select the ISO (for example, `debian-12.iso` or `Windows11.iso`).

5. Assign resources:
   - **Memory**: for example `2048 MB` (light Linux) or `4096+ MB` for Windows Server/Enterprise.  
   - **CPUs**: 2–4 vCPUs.  
   - **Disk**: at least 10–25 GB; for production or heavy workloads, 40–60 GB.

6. Configure the network:
   - In **Network Selection**, choose:  
     - `Virtual network 'default' : NAT` (built‑in NAT network),  
     or  
     - `Virtual network 'red_nat_lab' : NAT` (if you created a custom NAT network).

7. Confirm and create the VM:
   - Click **Finish**.  
   - The VM will boot, and the OS installation wizard will start.

---

## 4. Create a virtual machine with `virt-install` (terminal)

`virt-install` lets you create VMs directly from the terminal. This is useful for scripting, automation, or clear documentation of how each VM is built.

### 4.1 Example: light Linux VM with NAT network

```bash
virt-install \
  --name vm-web \
  --memory 2048 \
  --vcpus 2 \
  --disk size=10 \
  --os-variant debian12 \
  --network network=default,model=virtio \
  --cdrom /home/aleck/ISOs/debian-12.iso \
  --noautoconsole \
  --graphics spice
```

- `--name vm-web`: name of the virtual machine.  
- `--memory 2048`: 2 GB of RAM.  
- `--vcpus 2`: 2 virtual CPUs.  
- `--disk size=10`: 10 GB disk (QCOW2 by default).  
- `--os-variant debian12`: optimizes libvirt settings for Debian 12.  
- `--network network=default,model=virtio`: connects the VM to the `default` NAT network using a `virtio` network interface (better performance).  
- `--cdrom ...`: path to the installation ISO.  
- `--noautoconsole`: does not open the console after VM creation (you can connect later).  
- `--graphics spice`: uses SPICE for better graphics (recommended for desktop‑style VMs).

### 4.2 Basic management commands for VMs

After the VM is created:

Start the VM

virsh start vm-web
Shut down the VM gracefully

virsh shutdown vm-web
Force‑stop the VM (if it is not responding)

virsh destroy vm-web
List all VMs

virsh list --all
Connect to the VM console

virsh console vm-web

- `virsh list --all` shows:
  - `Name`, `State` (running/blocked/shut off), and `PID` for each VM.

---

## 5. Usage recommendations

### 5.1 NAT network and multiple VMs

- A NAT network (`default` or `red_nat_lab`) is a **shared logical network**:
  - You can attach **as many VMs** as you want.  
  - All VMs share the same gateway and DHCP range.  
  - They can access the Internet and talk to each other over the internal network.

### 5.2 Minimum recommended resources per VM type

| VM type                    | Minimum RAM | Minimum disk | Notes |
|----------------------------|-----------:|-------------:|-------|
| Light Linux (Debian/Ubuntu)| 1024–2048 MB | 10–20 GB   | Good for tests, small web servers, etc. |
| Windows 10/11 Enterprise   | 4096 MB        | 30–40 GB   | Requires more RAM/CPU for good desktop experience. |
| Windows Server 2022/2025   | 4096–8192 MB | 30–50 GB   | 6–8 GB RAM recommended for AD, IIS, etc. |

---

## 6. Quick setup checklist

- [ ] `libvirtd.service` is enabled and running.  
- [ ] Current user is in the `libvirt` group and session has been reloaded (log out / in or `newgrp libvirt`).  
- [ ] The NAT network `default` is started and set to `autostart`.  
- [ ] Essential packages installed: `qemu-full`, `libvirt`, `virt-manager`, `dnsmasq`, `iptables-nft`.  
- [ ] Installation ISOs are downloaded and the paths in `--cdrom` or `virt-manager` are correct.

---

This document is designed to serve as a **reference guide** for anyone working with your virtualization setup or maintaining your QEMU/KVM environment in the future.  
You can save it as `HOWTO-virtualization.md` and update VM names, ISO paths, or resource values to match your actual production/test environment.
END_COMMENT


# firefox: tree view tabs config:
# https://superuser.com/questions/1424478/can-i-hide-native-tabs-at-the-top-of-firefox
# and install your userChrome.css in the dot files...


# mypaint fix patches:
# https://github.com/wobbol/mypaint/commit/3b682d5898f4a6b709a2cd1a4d2b1b9288277cd6
# https://github.com/mypaint/mypaint/commit/2a92b6baf452aba2cff3cc0a7782b301da3933d7
# https://github.com/mypaint/mypaint/commit/243d9f450933c08077ce26e9626123cf69d241ba

# 7.1 Configuring graphical stuff
echo "Configuring graphical stuff..."
mkdir -p ~/.config/Thunar/
cp uca.xml ~/.config/Thunar/

# 7.2 Configuring youtube downloader for mp3
echo -e "\n# --- Media & Download Tools ---\n# Download best quality MP3 with metadata and thumbnail\nalias ytdl-mp3='yt-dlp -x --audio-format mp3 --audio-quality 0 --embed-thumbnail --add-metadata'" >> ~/.bashrc

# 8. Installing dev tools
echo "Installing dev tools..."
sudo pacman -S --noconfirm \
    clang texlive-basic texlive-latex texlive-latexrecommended texlive-latexextra \
    rustup crystal clisp sbcl nasm mingw-w64-gcc wine \
    emacs gdb global valgrind radare2 man-pages \
    pyenv openssh \
    calc

# 8.1 Setting up rustup
echo "Setting up rustup (stable toolchain)..."
rustup default stable

# 8.2 Configuring dev tools
echo "Configuring dev tools..."
mkdir -p ~/.emacs.d/emacs-backup
mkdir -p ~/.emacs.d/emacs-saves
mkdir -p ~/.emacs.d/themes
mkdir -p ~/.emacs.d/elpy/rpc-venv
cp init.el ~/.emacs.d/init.el
systemctl --user enable emacs
sudo cp ema /usr/bin/

# 8.3 Configuring ssh pair of keys
echo "Configuring ssh pair of keys..."
if [ ! -f ~/.ssh/id_rsa ]; then
    ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa -N ""
else
    echo "SSH key already exists at ~/.ssh/id_rsa, skipping."
fi

# 9. Installing steam, discord and xbox controller drivers
echo "Installing steam, discord and xbox controller drivers..."
sudo pacman -S --noconfirm noto-fonts-emoji discord ttf-liberation steam linux-headers dkms xpad

# 10. Configuring the hibernation
echo "Configuring the hibernation..."
echo "Available partitions:"
lsblk -o NAME,SIZE,TYPE,MOUNTPOINTS
RESUME_DEV=""
while [ -z "$RESUME_DEV" ]; do
    read -r -p "Enter the resume partition for hibernation (e.g. /dev/sda2): " RESUME_DEV
    if [ -z "$RESUME_DEV" ]; then
        echo "Partition cannot be empty. Please try again."
    elif [ ! -b "$RESUME_DEV" ]; then
        echo "'$RESUME_DEV' is not a valid block device. Please try again."
        RESUME_DEV=""
    fi
done
RESUME_DEV_ESCAPED=$(echo "$RESUME_DEV" | sed 's/\//\\\//g')
sudo sed -i "s/^GRUB_CMDLINE_LINUX=.*/GRUB_CMDLINE_LINUX=\"quiet resume=${RESUME_DEV_ESCAPED}\"/" /etc/default/grub
sudo grub-mkconfig -o /boot/grub/grub.cfg
echo "Hibernation resume set to $RESUME_DEV"

# 11. Installing the hp printer drivers (hplip, cups)
echo "Installing the hp printer drivers..."
sudo pacman -S --noconfirm hplip cups transmission-cli

# 11.1 Configuring the hp printer drivers (cups)
echo "Configuring the hp printer drivers (cups)..."
sudo systemctl enable cups
sudo systemctl start cups


# 12. Installing office software
echo "Installing office software..."
sudo pacman -S --noconfirm libreoffice-still

# 12.1 Install ms fonts
yay -S ttf-ms-fonts google-fonts-git apple-fonts


# 13. Installing network software
echo "Installing network software (openvpn)..."
yay -S --noconfirm openvpn

# 14. Rebooting
read -n1 -r -p "Press any key to reboot the system..." key
echo ""
echo "Removing install script..."
rm -- "$SCRIPT_PATH"
echo "Rebooting the system..."
sleep 2
reboot
