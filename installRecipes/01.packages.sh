#!/usr/bin/env bash

pnames=""

function checkP() {
    /usr/bin/pacman -Qs '^'$1'$' >/dev/null && echo "${GREEN}*${RESET} $1 ${GREEN}is installed${RESET}" || pnames="${pnames} $1"
}

echo ${GREEN}'Checking prerequisites packages to install:'${RESET}
echo
checkP zsh
checkP ripgrep
checkP fzf
checkP mpv
checkP xclip
checkP bat
checkP fd
checkP vifm
checkP gvim
checkP xterm
checkP terminus-font
checkP curl
checkP xorg-xrdb
checkP tmux
checkP rofi
# Convert TTF to OTF for pango
checkP fontforge
checkP python
# For AwesomeWM
checkP awesome
checkP gnome-keyring
checkP libsecret

if [ ! -z "${pnames}" ]; then
    echo ${RED}'Installing the following packages:'${RESET}
    echo ${pnames}
    yes | LC_ALL=en_US.UTF-8 sudo /usr/bin/pacman -Sy ${pnames}
else
    echo ${GREEN}'All packages are installed'${RESET}
fi
