#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '
export RDM=$(which rdm)
export PATH=/home/aleck/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl

export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="ema"         # $VISUAL opens in GUI mode

# Nodejs
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Java 
export PATH=/opt/jdk-17.0.11+9/bin:$PATH
export PATH=/opt/android-studio/bin:$PATH

# Android
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDORID_HOME/platform-tools

# Debuginfo
export DEBUGINFOD_URLS="https://debuginfod.archlinux.org"


# nnn
alias nnn='nnn -d'

