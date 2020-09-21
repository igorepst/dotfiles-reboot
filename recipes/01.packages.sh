#!/usr/bin/env bash

function doWork() {
    local pnames=""
    echo ${GREEN}'Checking prerequisites packages to install:'${RESET}
    echo

    local arr=(zsh ripgrep fzf mpv xclip bat fd xterm terminus-font curl xorg-xrdb tmux rofi)
    # Convert TTF to OTF for pango
    arr+=(fontforge python)
    # For AwesomeWM
    arr+=(awesome gnome-keyring libsecret)
    # Vim with clipboard support + coc.nvim
    arr+=(gvim nodejs)
    # Vifm + atool + img. support
    arr+=(vifm ueberzug ffmpegthumbnailer imagemagick poppler \
        atool rpm-tools bzip2 cpio gzip lha xz lzop p7zip tar unace unrar zip unzip)

    for i in "${arr[@]}"; do
        /usr/bin/pacman -Qs '^'$i'$' >/dev/null && echo "${GREEN}*${RESET} $i ${GREEN}is installed${RESET}" || pnames="${pnames} $i"
    done
    if [ ! -z "${pnames}" ]; then
        echo ${RED}'Installing the following packages:'${RESET}
        echo ${pnames}
        yes | LC_ALL=en_US.UTF-8 sudo /usr/bin/pacman -Sy ${pnames}
    else
        echo ${GREEN}'All packages are installed'${RESET}
    fi
}

doWork
