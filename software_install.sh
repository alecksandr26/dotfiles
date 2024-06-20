#!/bin/env bash

# 1. Downloading my dotfiles
echo "Downloading the dotfiles..."
sleep 2
cd Documents/
(echo y) | sudo pacman -S git
git clone https://github.com/alecksandr26/dotfiles
cd dotfiles/

# 2. Installing Xorg and I3 (dmenu) The window server
echo "Installing Xorg and I3 (dmenu)..."
sleep 2
(
    echo ""
    echo ""
    echo ""
    echo ""
    echo y
) | sudo pacman -S xorg xorg-xinit i3 dmenu 

# 2.1. Installing drivers
echo "Installing drivers..."
sleep 2
(
    echo y
) | sudo pacman -S xf86-video-amdgpu mesa vulkan-radeon libva-mesa-driver mesa-vdpau

# 3. Configure Xorg and I3
echo "Configurering Xorg and I3..."
sleep 2
cp .xinitrc ~
cp .Xresources ~
sudo cp 20-amdgpu.conf /etc/X11/xorg.conf.d/
mkdir -p ~/.config/
mkdir -p ~/.config/i3/
cp config ~/.config/i3/

# 3. Pulse Audio The server audio and Alsa drivers
echo "Installing Pulse Audio..."
sleep 2
(
    echo "y"
) | sudo pacman -S sof-firmware alsa-firmware pulseaudio pulseaudio-alsa pulsemixer pavucontrol


# Basic stuff: Terminal, Text editor, Web browser, File manager, Image and video viewer
# 4. Installing my terminal stuff
echo "Installing basic stuff (Kitty, Temux, Nano, Vim, nnn)..."
sleep 2
(
    echo "y"
) | sudo pacman -S kitty tmux nano vim nnn

# Guides for the weird software:
# nnn: https://opensource.com/article/22/12/linux-file-manager-nnn

# 4.1 Configure the terminal
echo "Configuring basic stuff (Kitty, Temux, Nano, Vim, nnn)..."
sleep 2
cp .bashrc ~
cp kitty.conf ~/.config/kitty/
cp .nanorc ~
cp .vimrc ~

# 5. Installing Dev Tools
echo "Installing Dev Tools..."
sleep 2
(
    echo "y"
) | sudo pacman -S base-devel







