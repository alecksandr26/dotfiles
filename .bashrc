#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Git branch for prompt
__git_branch() {
    local branch
    branch=$(git symbolic-ref --short HEAD 2>/dev/null)
    [ -n "$branch" ] && echo " ($branch)"
}

# Colored prompt (green user@host, blue dir, yellow git branch)
PS1='\[\e[1;32m\]\u@\h\[\e[0m\] \[\e[1;34m\]\W\[\e[1;33m\]$(__git_branch)\[\e[0m\] $ '

# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias nnn='nnn -d'

# Editor
export EDITOR="emacsclient -t"
export VISUAL="ema"

# Debuginfo
export DEBUGINFOD_URLS="https://debuginfod.archlinux.org"

# pyenv
if command -v pyenv &> /dev/null; then
    eval "$(pyenv init - bash)"
fi
