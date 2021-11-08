#!/usr/bin/env bash

function doWork(){
    export RED='\033[0;31m'
    export GREEN='\033[0;32m'
    export RESET='\033[0m'

    printf "${GREEN}Checking OS${RESET}\n"
    OS_ID=$(sed -ne 's/^ID=\(.*\)/\1/p' /etc/os-release)
    case "$OS_ID" in
        arch|ubuntu) export OS_ID;;
        *) 
            printf "${RED}Unsupported OS: ${OS_ID}${RESET}\n"
            exit 1;;
    esac

    printf "${GREEN}Installing common configuration${RESET}\n"

    DOTFILES_DIR="$(
    cd "$(dirname "$0")"
    pwd
    )"
    export DOTFILES_DIR
    pushd "${DOTFILES_DIR}" >/dev/null

    MY_PC_IS=
    local MY_PC_ANSWER_FILE="${HOME}/.cache/.my_pc_is"
    [ -f ${MY_PC_ANSWER_FILE} ] && source ${MY_PC_ANSWER_FILE}
    if [ -z ${MY_PC_IS} ]; then
        local MY_PC_IS_H
        printf "\n${RED}Checking environment${RESET}\n"
        read -p "Is it a home (h) or work (w) PC? " MY_PC_IS_H
        if [[ "${MY_PC_IS_H}" =~ ^[Hh]$ ]]; then
            MY_PC_IS="home"
        elif [[ "${MY_PC_IS_H}" =~ ^[Ww]$ ]]; then
            MY_PC_IS="work"	
        fi
        echo 'export MY_PC_IS='${MY_PC_IS}>${MY_PC_ANSWER_FILE}
    fi
    export MY_PC_IS

    local RECIPES_DIR="${DOTFILES_DIR}/recipes"
    for recipe in ${RECIPES_DIR}/[0-9]*.sh; do
        if [ -x "${recipe}" ]; then
            printf "${GREEN}*********************${RESET}\n"
            "${recipe}"
        fi
    done

    popd >/dev/null
    printf "\n${GREEN}Setting ${MY_PC_IS} environment finished. Please reboot${RESET}\n"
}

doWork
