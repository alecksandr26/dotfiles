#!/bin/env bash

su root

# 1. Downloading my dotfiles
echo "Downloading the dotfiles..."
sleep 2
cd ./Documents
(echo y) | pacman -S git
git clone https://github.com/alecksandr26/dotfiles
cd ./dotfiles


# 2. Installing Xorg and I3 (dmenu) The window server
echo "Installing Xorg and I3 (dmenu)..."
sleep 2
(
    echo ""
    echo ""
    echo y
    echo y
    echo ""
    echo ""
    echo y
    echo y
) | pacman -S xorg xorg-xinit i3 dmenu 

# 2.1. Installing drivers
echo "Installing drivers..."
sleep 2
(
    echo y
    echo y
    echo y
) | pacman -S xf86-video-amdgpu mesa vulkan-radeon

# 3. Configure Xorg and I3
echo "Configurering Xorg and I3..."
sleep 2
cp .xinitrc ../../
mkdir -p .config
mkdir -p .config/i3


# 3. Pulse Audio The server audio and Alsa drivers



# 4. Basic stuff: Terminal, Text editor, Web browser, File manager, Image and video viewer









