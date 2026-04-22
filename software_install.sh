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
sudo pacman -S --noconfirm lightdm lightdm-gtk-greeter
sudo systemctl enable lightdm


# 3.4 Auto-configure LightDM Monitor
echo "Configuring LightDM monitor detection..."

# Detect connected monitors using the kernel (works in TTY)
echo "Detecting connected monitors via kernel..."
CONNECTED_MONITORS=$(ls /sys/class/drm/card0-* | grep -e "status" | xargs grep -l "^connected" | awk -F'/' '{print $5}' | sed 's/card0-//;s/-//' | tr '[:lower:]' '[:upper:]')
# Note: The above logic cleans up names like 'card0-DP-2' to 'DP2'. 
# However, xrandr usually needs the hyphenated name like 'DisplayPort-2'.

echo "------------------------------------------------------------"
echo "NOTE: Use 'DisplayPort-2' if your setup hasn't changed."
echo "------------------------------------------------------------"

read -r -p "Enter your primary monitor name [Default: DisplayPort-2]: " PRIMARY_MONITOR
PRIMARY_MONITOR=${PRIMARY_MONITOR:-DisplayPort-2}
sudo tee /etc/lightdm/display-setup.sh > /dev/null <<EOF
#!/bin/sh
sleep 2
PRIMARY=$PRIMARY_MONITOR
xrandr --output \$PRIMARY --primary
EOF
sudo chmod +x /etc/lightdm/display-setup.sh

# Update lightdm.conf to use the script under the [Seat:*] section
# We ensure the script is enabled even if the line was missing
if grep -q "^\[Seat:\*\]" /etc/lightdm/lightdm.conf; then
    sudo sed -i '/^\[Seat:\*\]/a display-setup-script=/etc/lightdm/display-setup.sh' /etc/lightdm/lightdm.conf
else
    echo -e "\n[Seat:*]\ndisplay-setup-script=/etc/lightdm/display-setup.sh" | sudo tee -a /etc/lightdm/lightdm.conf
fi

# 3.4.1 Early KMS for AMD (Fixes LightDM black screens)
echo "Enabling Early KMS for amdgpu..."
sudo sed -i 's/^MODULES=(/MODULES=(amdgpu /' /etc/mkinitcpio.conf
sudo mkinitcpio -P

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

# 5.1 Bluetooth audio and things
echo "Installing Blueman for bluetooth..."
sudo pacman -S --noconfirm bluez bluez-utils blueman
sudo systemctl enable bluetooth.service

# 6. Installing terminal emulator stuff
echo "Installing terminal emulator stuff..."
sudo pacman -S --noconfirm kitty tmux nano vim nnn less htop zip unzip xdg-utils tree adobe-source-code-pro-fonts 
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
    chromium thunar file-roller gvfs tumbler ffmpegthumbnailer pavucontrol libdvdcss vlc feh mypaint yt-dlp \
    xclip maim \
    obs-studio audacity \
    kdenlive gimp \
    qemu-full



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
