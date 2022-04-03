#!/usr/bin/env bash

function checkp() {
    case "${1}" in
        arch) /usr/bin/pacman -Qs '^'"${2}"'$' > /dev/null ;;
        ubuntu) /usr/bin/dpkg -s "${2}" > /dev/null 2>&1 ;;
    esac
}

function doWork() {
    local RED='\033[0;31m'
    local GREEN='\033[0;32m'
    local RESET='\033[0m'

    local OS_ID arr
    printf "${GREEN}Checking OS${RESET}\n"
    OS_ID=$(sed -ne 's/^ID=\(.*\)/\1/p' /etc/os-release)
    case "${OS_ID}" in
        arch) arr=();;
        ubuntu) arr=(libncursesw5-dev libmagic-dev);;
        *) 
            printf "${RED}Unsupported OS: ${OS_ID}${RESET}\n"
            exit 1;;
    esac
    local pnames=""
    printf "${GREEN}Checking prerequisites packages to install:${RESET}\n\n"
    for i in "${arr[@]}"; do
        checkp "${i}" && printf "${GREEN}*${RESET} $i ${GREEN}is installed${RESET}\n" || pnames="${pnames} $i"
    done
    if [ -n "${pnames}" ]; then
        printf "${RED}Installing the following packages:${RESET}\n${pnames}\n"
        case "${OS_ID}" in
            arch) yes | sudo /usr/bin/pacman -Sy ${pnames} ;;
            ubuntu) sudo /usr/bin/apt-get update && sudo /usr/bin/apt-get install -y ${pnames} ;;
        esac
    else
        printf "${GREEN}All packages are installed${RESET}\n"
    fi
    
    local dir=~/github/vifm
    if [ -d "${dir}" ]; then
        pushd "${dir}" >/dev/null || exit 1
        git reset --hard
        if ! git pull origin master; then
            printf "${RED}Make failed${RESET}\n"
            popd >/dev/null || exit 1
            exit 1
        fi
    else
        mkdir -p "${dir}"
        if ! git clone https://github.com/vifm/vifm.git "${dir}"; then
            printf "${RED}Git clone failed${RESET}\n"
            popd >/dev/null || exit 1
            exit 1
        fi
        pushd "${dir}" >/dev/null || exit 1
    fi

    CFLAGS+=' -fcommon'
    if ! ./configure; then
        printf "${RED}Configure failed${RESET}\n"
        popd >/dev/null || exit 1
        exit 1
    fi
    if ! make; then
        printf "${RED}Make failed${RESET}\n"
        popd >/dev/null || exit 1
        exit 1
    fi

    local inst_dir=~/.zsh/volatile/igorepst/_gh_release/vifm/vifm
    local bin_dir=~/.zsh/volatile/igorepst/_gh_release/_cache/_bin
    local compl_dir=~/.zsh/volatile/igorepst/_gh_release/_cache/_compl
    mkdir -p "${inst_dir}"
    cp -r data src/vifm src/vifm-pause data/shell-completion/zsh/_vifm "${inst_dir}"
    ln -sf "${inst_dir}/vifm" "${bin_dir}/vifm" 
    ln -sf "${inst_dir}/vifm-pause" "${bin_dir}/vifm-pause" 
    ln -sf "${inst_dir}/_vifm" "${compl_dir}/_vifm" 
    rm -f ~/.zsh/volatile/zcompdump* 2>/dev/null
    
    popd >/dev/null || exit 1
    exec zsh
}


doWork
