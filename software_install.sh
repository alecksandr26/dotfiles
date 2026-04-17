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

# 4. Configure xorg and xfce
echo "Configuring xorg and xfce..."
cp .xinitrc ~
cp .Xresources ~
mkdir -p ~/.config/
sudo mkdir -p /usr/share/themes/empty/xfwm4/
sudo touch /usr/share/themes/empty/xfwm4/themerc

# 5. PipeWire audio server and firmware
echo "Installing PipeWire..."
sudo pacman -S --noconfirm sof-firmware alsa-firmware pipewire pipewire-alsa pipewire-pulse wireplumber

# 6. Installing terminal emulator stuff
echo "Installing terminal emulator stuff..."
sudo pacman -S --noconfirm kitty tmux nano vim nnn less htop zip unzip xdg-utils tree adobe-source-code-pro-fonts
yay -S --noconfirm timg

# Guides for the weird software:
# nnn: https://opensource.com/article/22/12/linux-file-manager-nnn
# nnn: Usage https://github.com/jarun/nnn/wiki/Usage#keyboard-mouse

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
    chromium thunar gvfs tumbler ffmpegthumbnailer pavucontrol libdvdcss vlc feh mypaint \
    xclip maim \
    obs-studio audacity \
    kdenlive gimp \
    qemu-full

# 7.1 Configuring graphical stuff
echo "Configuring graphical stuff..."
mkdir -p ~/.config/Thunar/
cp uca.xml ~/.config/Thunar/

# 8. Installing dev tools
echo "Installing dev tools..."
sudo pacman -S --noconfirm \
    clang texlive-basic texlive-latex texlive-latexrecommended texlive-latexextra \
    rustup crystal clisp sbcl nasm mingw-w64-gcc wine \
    emacs gdb global valgrind radare2 man-pages \
    pyenv openssh

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

# 12. Installing network software
echo "Installing network software (openvpn)..."
yay -S --noconfirm openvpn

# 13. Rebooting
read -n1 -r -p "Press any key to reboot the system..." key
echo ""
echo "Removing install script..."
rm -- "$SCRIPT_PATH"
echo "Rebooting the system..."
sleep 2
reboot
