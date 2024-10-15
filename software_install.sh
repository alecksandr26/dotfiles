#!/bin/env bash

# 1. Installing Yay
echo "Installing Yay..."
sleep 2
(echo y) | sudo pacman -S git base-devel cmake
cd ~/Downloads
git clone https://aur.archlinux.org/yay.git
cd yay/
(
    echo "y"
    echo "y"
) | makepkg -si


# 2. Downloading my dotfiles
echo "Downloading the dotfiles..."
sleep 2
cd ~/Documents/
git clone https://github.com/alecksandr26/dotfiles
cd dotfiles/


# 3. Installing xorg and i3 (dmenu) The window server
echo "Installing xorg and i3 (dmenu)..."
sleep 2
(
    echo ""
    echo ""
    echo ""
    echo ""
    echo y
) | sudo pacman -S xorg xorg-xinit i3 dmenu picom

# 3.1. Installing drivers
echo "Installing drivers..."
sleep 2
(
    echo y
) | sudo pacman -S xf86-video-amdgpu mesa vulkan-radeon libva-mesa-driver mesa-vdpau libva-vdpau-driver libva-utils 

# 3.2 Enabling the multilib
echo "Enabling the multilib..."
sleep 2
sudo sed -i "/\[multilib\]/,/Include/"'s/^#//' /etc/pacman.conf
sudo pacman -Sy
(
    echo y
) | sudo pacman -S lib32-mesa lib32-vulkan-radeon


# 4. Configure xorg and i3
echo "Configurering xorg and I3..."
sleep 2
cp .xinitrc ~
cp .Xresources ~
sudo cp 20-amdgpu.conf /etc/X11/xorg.conf.d/
mkdir -p ~/.config/
mkdir -p ~/.config/i3/
cp config ~/.config/i3/
mkdir -p ~/.config/picom/
cp picom.conf ~/.config/picom/

# 5. Pulse Audio The server audio and Alsa drivers
echo "Installing Pulse Audio..."
sleep 2
(
    echo "y"
) | sudo pacman -S sof-firmware alsa-firmware pulseaudio pulseaudio-alsa pulsemixer

# pulsemixer: https://github.com/GeorgeFilipkin/pulsemixer

# 6. Installing terminal emulator stuff
echo "Installing terminal emulator stuff (Kitty, Tmux, Nano, Vim, Nnn, Less, Htop)..."
sleep 2
(
    echo "y"
) | sudo pacman -S kitty tmux nano vim nnn less htop zip unzip xdg-utils
(
    echo "1"
    echo "N"
    echo "y"
) | yay -S timg

# Guides for the weird software:
# nnn: https://opensource.com/article/22/12/linux-file-manager-nnn
# nnn: Usage https://github.com/jarun/nnn/wiki/Usage#keyboard-mouse

# 6.1 Configure the terminal emulator stuff
echo "Configuring terminal emulator stuff (Kitty, Tmux, Nano, Vim, nnn)..."
sleep 2
cp .bashrc ~/
mkdir -p ~/.config/kitty
cp kitty.conf ~/.config/kitty/
cp .nanorc ~/
cp .vimrc ~/

# 7. Installing basic graphics tools (chromium, thunar, pavucontrol, vlc)...
echo "Installing basic graphics tools (chromium, thunar, pavucontrol, vlc)..."
sleep 2
(
    echo ""
    echo "y"
) | sudo pacman -S chromium thunar gvfs tumbler ffmpegthumbnailer pavucontrol libdvdcss vlc feh mypaint

# 7.1 Configuring graphical stuff
echo "Configuring graphical stuff..."
sleep 2
mkdir -p ~/.config/Thunar/
cp uca.xml ~/.config/Thunar/


# 8. Installing dev tools
echo "Installing dev tools..."
sleep 2
(
    echo "y"
) | sudo pacman -S clang texlive-latex texlive-latexrecommended texlive-latexextra rust crystal clisp sbcl nasm mingw-w64-gcc wine emacs gdb global valgrind radare2 man-pages


# 8.1 Configuring dev tools
echo "Configuring dev tools..."
sleep 2
cp .emacs ~/
systemctl --user enable emacs
sudo cp ema /usr/bin/

# 8.2 Configuring ssh pair of keys
echo "Installing openssh..."
sleep 2
(
    echo "y"
) | sudo pacman -S openssh

echo "Configuring ssh pair of keys..."
sleep 2
(
    echo ""
    echo ""
    echo ""
) | ssh-keygen -t rsa -b 4096


# 9. Installing basic utilies (xclip, maim)
echo "Installing basic utilies (xclip, maim)..."
sleep 2
(
    echo "y"
) | sudo pacman -S xclip maim

# 10. Installing recording tools (obs-studio, audacity)
echo "Installing recording tools (obs-studio, audacity)..."
sleep 2
(
    echo "y"
) | sudo pacman -S obs-studio audacity

# 11. Installing editing software
echo "Installing editing software"
sleep 2
(
    echo "1"
    echo "y"
) | sudo pacman -S kdenlive gimp

# 12. Installing virtualization software
echo "Installing virtualization software (qemu)..."
sleep 2
(
    echo "y"
) | sudo pacman -S qemu-full

# 13. Installing steam, discord and xbox controller drivers 
echo "Installing steam, discord and xbox controller drivers..."
sleep 2
(
    echo "y"
) | sudo pacman -S noto-fonts-emoji discord ttf-liberation steam linux-headers dkms xpad

# 13.1 Cofniguing the drivers of xbox controller
# echo "Configuing the drivers of xbox controller..."
# sudo modprobe xpad


# 14. Configuring the hibernation
echo "Configuring the hibernation..."
sudo sed -i 's/^GRUB_CMDLINE_LINUX=.*/GRUB_CMDLINE_LINUX="quiet resume=\/dev\/sda2"/' /etc/default/grub


# 15. Installing the hp printer drivers (hlip, cups)
echo "Installing the hp printer drivers (hlip, cups)..."
sleep 2
(
    echo "y"
) | sudo pacman -S hplip cups

# 15.1 Configuring the hp printer drivers (cups)
echo "Configuring the hp printer drivers (cups)..."
sleep 2
sudo systemctl enable cups
sudo systemctl start cups


# 16. Installing torrent software (transmission-cli)...
echo "Installing torrent software (transmission-cli)..."
sleep 2
(
    echo "y"
) | sudo pacman -S transmission-cli


# 17. Installing network software
echo "Installing network software (net-tools, openvpn)..."
sleep 2
(
    echo "y"
) | sudo pacman -S net-tools
sleep 2
(
    echo "y"
) | yay -S openvpn


# 18. Rebooting
read -n1 -r -p "Press any key to reboot the system..." key
echo "Removing $0..."
sleep 2
rm ~/$0
echo "Rebooting the system..."
sleep 2

reboot



