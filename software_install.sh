#!/bin/env bash

# 1. Installing Yay
echo "Installing Yay..."
sleep 2
(echo y) | sudo pacman -S git base-devel cmake
cd ~/Downloads
git clone https://aur.archlinux.org/yay.git
cd yay/
makepkg -si


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

# 6.1 Configure the terminal emulator stuff
echo "Configuring terminal emulator stuff (Kitty, Tmux, Nano, Vim, nnn)..."
sleep 2
cp .bashrc ~/
mkdir -p ~/.config/kitty
cp kitty.conf ~/.config/kitty/
cp .nanorc ~/
cp .vimrc ~/

# 7. Installing basic graphics tools (chromium, thunar, pavucontrol, vlc)
echo "Installing basic graphics tools (chromium, thunar, pavucontrol, vlc)..."
sleep 2
(
    echo ""
    echo "y"
) | sudo pacman -S chromium thunar gvfs tumbler ffmpegthumbnailer pavucontrol vlc feh mypaint

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
) | sudo pacman -S clang texlive-latex texlive-latexrecommended texlive-latexextra rust crystal clisp sbcl nasm mingw-w64-gcc wine emacs


# 8.1 Configuring dev tools
echo "Configuring dev tools..."
sleep 2
cp .emacs ~/
systemctl --user enable emacs
sudo cp ema /usr/bin/


# 9. Installing basic utilies (xclip, scrot)
echo "Installing basic utilies (xclip, scrot)..."
sleep 2
(
    echo "y"
) | sudo pacman -S xclip scrot

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
    echo "y"
) | sudo pacman -S kdenlive gimp

# 12. Installing virtualization software
echo "Installing virtualization software (qemu)..."
sleep 2
(
    echo "y"
) | sudo pacman -S qemu-full

# 13. Installing steam and discord
echo "Installing steam and discord..."
sleep 2
(
    echo "y"
) | sudo pacman -S noto-fonts-emoji discord ttf-liberation steam

# 14. Rebooting
read -n1 -r -p "Press any key to reboot the system..." key
echo "Rebooting the system..."
sleep 2
reboot






