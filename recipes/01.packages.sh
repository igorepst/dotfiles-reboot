#!/usr/bin/env bash

function checkp() {
    case "${OS_ID}" in
        arch) /usr/bin/pacman -Qs '^'"${1}"'$' >/dev/null;;
        ubuntu) /usr/bin/dpkg -s "${1}" >/dev/null 2>&1;;
    esac
}

function doWork() {
    local pnames=""
    printf "${GREEN}Checking prerequisites packages to install:${RESET}\n\n"

    local arr=(git zsh mpv xclip curl jq fontforge npm imagemagick p7zip) 

    local add_arr
    case "${OS_ID}" in
        arch) add_arr=(python terminus-font xorg-xrdb);;
        ubuntu) add_arr=(python3 fonts-terminus x11-xserver-utils gnome-shell-extension-dash-to-panel p7zip-full p7zip-rar)
            if ! grep -q "^deb .*git-core" "/etc/apt/sources.list" "/etc/apt/sources.list.d/*" 2>/dev/null; then
                printf '%sAdding official Git PPA and updating...%s\n' "${GREEN}" "${RESET}"
                sudo add-apt-repository -yu ppa:git-core/ppa && sudo apt-get install -y git
            fi
            ;;
    esac

    arr=("${arr[@]}" "${add_arr[@]}")
    #local arr=(rofi acpilight picom)
    # Vifm + atool + img. support
    #arr+=(vifm ueberzug ffmpegthumbnailer imagemagick poppler mediainfo \
    #    atool rpm-tools bzip2 cpio gzip lha xz lzop p7zip tar unace unrar zip unzip)

    for i in "${arr[@]}"; do
        checkp "${i}" && printf "${GREEN}*${RESET} $i ${GREEN}is installed${RESET}\n" || pnames="${pnames} $i"
    done
    if [ -n "${pnames}" ]; then
        printf "${RED}Installing the following packages:${RESET}\n${pnames}\n"
        case "${OS_ID}" in
            arch) yes | sudo /usr/bin/pacman -Sy ${pnames};;
            ubuntu) sudo /usr/bin/apt-get update && sudo /usr/bin/apt-get install -y ${pnames};;
        esac
    else
        printf "${GREEN}All packages are installed${RESET}\n"
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
                printf "${RED}Installing or updating the following package from AUR: ${dir}${RESET}\n"
            else
                popd >/dev/null
            fi
        else
            printf "${RED}Installing or updating the following package from AUR: ${dir}${RESET}\n"
            git clone "${url}"
            pushd "${dir}" >/dev/null
            changed=1
        fi
        if [ "${changed}" = "1" ]; then
            makepkg -si
            popd >/dev/null
        else
            printf "${GREEN}The following package from AUR is up to date: ${dir}${RESET}\n"
        fi
    done
    popd >/dev/null
}

function change_shell() {
    printf "${GREEN}Checking current shell${RESET}\nCurrent shell is ${SHELL}\n"
    [[ "${SHELL}" = '/bin/zsh' ]] && return 0
    printf "Changing shell to /bin/zsh\n"
    chsh -s /bin/zsh
}

doWork
doAurWork
change_shell
