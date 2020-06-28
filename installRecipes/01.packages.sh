#!/usr/bin/env bash

pnames=""

function checkP() {
    /usr/bin/pacman -Qs '^'$1'$' >/dev/null && echo "$1 is installed" || pnames="$pnames $1"
}

echo 'Checking prerequisites packages to install:'
checkP zsh
checkP ripgrep
checkP fzf
checkP mpv
checkP xclip
checkP bat
checkP fd
checkP vifm
checkP awesome
checkP gvim
checkP xterm
checkP terminus-font
# Convert TTF to OTF for pango
checkP fontforge
checkP python

if [ ! -z "$pnames" ]; then
    echo 'Installing the following packages:'
    echo $pnames
    sudo /usr/bin/pacman -Sy $pnames --noconfirm
else
    echo 'All the packages are installed'
fi
