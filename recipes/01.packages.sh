#!/usr/bin/env bash

function doWork() {
    local pnames=""
    echo ${GREEN}'Checking prerequisites packages to install:'${RESET}
    echo

    # 'jq' is for JSON manipulation, always useful
    local arr=(zsh ripgrep fzf mpv xclip bat fd xterm terminus-font curl xorg-xrdb tmux rofi jq kitty)
    # Convert TTF to OTF for pango
    arr+=(fontforge python)
    # Vim with clipboard support + coc.nvim
    arr+=(gvim nodejs)
    # Vifm + atool + img. support
    arr+=(vifm ueberzug ffmpegthumbnailer imagemagick poppler mediainfo \
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

function doAurWork() {
    mkdir -p ~/aur
    pushd ~/aur >/dev/null
    local arr=('https://aur.archlinux.org/vifmimg-git.git')
    local dir
    local changed
    for url in "${arr[@]}"; do
        dir=${url##*\/}
        dir=${dir%%\.git*}
        if [ -d "${dir}" ]; then
            pushd "${dir}" >/dev/null
            git pull origin master 2>&1 | grep -q 'Already up to date' || changed=1
            if [ "${changed}" = "1" ]; then
                echo ${RED}'Installing or updating the following package from AUR: '${dir}${RESET}
            else
                popd >/dev/null
            fi
        else
            echo ${RED}'Installing or updating the following package from AUR: '${dir}${RESET}
            git clone "${url}"
            pushd "${dir}" >/dev/null
            changed=1
        fi
        if [ "${changed}" = "1" ]; then
            makepkg -si
            popd >/dev/null
        else
            echo ${GREEN}'The following package from AUR is up to date: '${dir}${RESET}
        fi
    done
    popd >/dev/null
}

doWork
doAurWork
