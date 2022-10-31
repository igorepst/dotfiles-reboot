#!/usr/bin/env bash

function checkp() {
    case "${OS_ID}" in
        arch) /usr/bin/pacman -Qs '^'"${1}"'$' > /dev/null ;;
        ubuntu) /usr/bin/dpkg -s "${1}" > /dev/null 2>&1 ;;
    esac
}

function doWork() {
    local pnames=""
    printf "${GREEN}Checking prerequisites packages to install:${RESET}\n\n"

    local arr=(git zsh mpv xclip curl jq npm imagemagick p7zip mediainfo evince rsync gnome-screenshot)

    local add_arr
    case "${OS_ID}" in
        arch) add_arr=(polkit python python-pip terminus-font xorg-server xorg-xrdb xorg-xinit unzip ttf-dejavu upower acpilight picom noto-fonts ilock xorg-xrandr autorandr hsetroot intel-media-driver) ;;
        ubuntu)
            add_arr=(python3 python3-pip fonts-terminus x11-xserver-utils gnome-shell-extension-dash-to-panel p7zip-full p7zip-rar fonts-noto)
            if ! grep -q "^deb .*git-core" "/etc/apt/sources.list" "/etc/apt/sources.list.d/*" 2> /dev/null; then
                printf '%sAdding official Git PPA and updating...%s\n' "${GREEN}" "${RESET}"
                sudo add-apt-repository -yu ppa:git-core/ppa && sudo apt-get install -y git
            fi
            ;;
    esac

    arr=("${arr[@]}" "${add_arr[@]}")
    for i in "${arr[@]}"; do
        checkp "${i}" && printf "${GREEN}*${RESET} $i ${GREEN}is installed${RESET}\n" || pnames="${pnames} $i"
    done
    if [ -n "${pnames}" ]; then
        printf "${RED}Installing the following packages:${RESET}\n${pnames}\n"
        case "${OS_ID}" in
            arch) sudo /usr/bin/pacman -Sy --noconfirm ${pnames} ;;
            ubuntu) sudo /usr/bin/apt-get update && sudo /usr/bin/apt-get install -y ${pnames} ;;
        esac
    else
        printf "${GREEN}All packages are installed${RESET}\n"
    fi
}

function doAurWork() {
    [[ "${OS_ID}" != "arch" ]] && return 0
    mkdir -p ~/aur
    pushd ~/aur > /dev/null
    local arr=('https://aur.archlinux.org/awesome-git.git')
    local dir
    local changed
    for url in "${arr[@]}"; do
        dir=${url##*\/}
        dir=${dir%%\.git*}
        if [ -d "${dir}" ]; then
            pushd "${dir}" > /dev/null
            git pull origin master 2>&1 | grep -q 'Already up to date' || changed=1
            if [ "${changed}" = "1" ]; then
                printf "${RED}Installing or updating the following package from AUR: ${dir}${RESET}\n"
            else
                popd > /dev/null
            fi
        else
            printf "${RED}Installing or updating the following package from AUR: ${dir}${RESET}\n"
            git clone "${url}"
            pushd "${dir}" > /dev/null
            changed=1
        fi
        if [ "${changed}" = "1" ]; then
            makepkg -si
            popd > /dev/null
        else
            printf "${GREEN}The following package from AUR is up to date: ${dir}${RESET}\n"
        fi
    done
    popd > /dev/null
}

function change_shell() {
    printf "${GREEN}Checking current shell${RESET}\nCurrent shell is ${SHELL}\n"
    [[ "${SHELL}" = '/bin/zsh' ]] && return 0
    printf "Changing shell to /bin/zsh\n"
    chsh -s /bin/zsh
}

function install_go() {
    if ! command -v go > /dev/null; then
        printf "${GREEN}Installing Golang${RESET}\n"
        local godown='https://go.dev/dl/'
        local ver
        ver=$(curl -s ${godown} 2>&1 | grep -Po '<span class="filename">\K.*?(?=.linux)')
        if [ -z "${ver}" ]; then
            printf "${RED}Cannot find latest Golang version${RESET}\n"
            return
        fi
        local out=/tmp/golinst
        if ! curl -k -L "${godown}${ver}.linux-amd64.tar.gz" -o ${out}; then
            printf "${RED}Cannot download latest Golang version ${ver}${RESET}\n"
            return
        fi
        tar -xf "${out}" -C "${HOME}"
        rm -f ${out}
    fi
    printf "${GREEN}Golang is installed${RESET}\n"
    ~/go/bin/go version
}

doWork
doAurWork
change_shell
install_go
