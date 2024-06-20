#!/bin/env bash

su root

# 1. Installing Xorg and I3 (dmenu, feh) The window server
(
    echo all
    echo y
) | pacman -S xorg i3



# 2. Pulse Audio The server audio and Alsa drivers

# 3. Basic stuff: Terminal, Text editor, Web browser, File manager, Image and video viewer









