#!/usr/bin/env bash

function checkp() {
    case "${OS_ID}" in
        arch) /usr/bin/pacman -Qs '^'"${1}"'$' >/dev/null;;
        ubuntu) /usr/bin/dpkg-query -l "${1}" >/dev/null;;
    esac
}

function doWork() {
    local pnames=""
    echo ${GREEN}'Checking prerequisites packages to install:'${RESET}
    echo

    local arr=(git zsh mpv xclip curl jq fontforge npm)

    local add_arr
    case "${OS_ID}" in
        arch) add_arr=(python terminus-font xorg-xrdb);;
        ubuntu) add_arr=(python3 fonts-terminus x11-xserver-utils gnome-shell-extension-dash-to-panel);;
    esac

    arr=("${arr[@]}" "${add_arr[@]}")
    #local arr=(rofi acpilight picom)
    # Vifm + atool + img. support
    #arr+=(vifm ueberzug ffmpegthumbnailer imagemagick poppler mediainfo \
    #    atool rpm-tools bzip2 cpio gzip lha xz lzop p7zip tar unace unrar zip unzip)

    for i in "${arr[@]}"; do
        checkp "${i}" && echo "${GREEN}*${RESET} $i ${GREEN}is installed${RESET}" || pnames="${pnames} $i"
    done
    if [ ! -z "${pnames}" ]; then
        echo ${RED}'Installing the following packages:'${RESET}
        echo ${pnames}
        case "${OS_ID}" in
            arch) yes | sudo /usr/bin/pacman -Sy ${pnames};;
            ubuntu) sudo /usr/bin/apt-get update && sudo /usr/bin/apt-get install -y ${pnames};;
        esac
    else
        echo ${GREEN}'All packages are installed'${RESET}
    fi
}

function doAurWork() {
    [[ "${OS_ID}" != "arch" ]] && return 0
    mkdir -p ~/aur
    pushd ~/aur >/dev/null
    local arr=('https://aur.archlinux.org/awesome-git.git')
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

function change_shell() {
    echo ${GREEN}'Checking current shell'${RESET}
    echo "Current shell is ${SHELL}"
    [[ "${SHELL}" = '/bin/zsh' ]] && return 0
    echo 'Changing shell to /bin/zsh'
    chsh -s /bin/zsh
}

doWork
doAurWork
change_shell
