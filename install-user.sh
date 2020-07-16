#!/usr/bin/env bash

function doWork(){
    export RED=$(tput setaf 1)
    export GREEN=$(tput setaf 2)
    export RESET=$(tput sgr0)

    echo ${GREEN}'Installing common configuration'${RESET}

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
        echo
        echo ${RED}'Checking environment'${RESET}
        read -p "Is it a home (h), work (w) PC or virtual machine (v)? " MY_PC_IS_H
        if [[ "${MY_PC_IS_H}" =~ ^[Hh]$ ]]; then
            MY_PC_IS="home"
        elif [[ "${MY_PC_IS_H}" =~ ^[Ww]$ ]]; then
            MY_PC_IS="work"	
        else
            MY_PC_IS="vm"
        fi
        echo 'export MY_PC_IS='${MY_PC_IS}>${MY_PC_ANSWER_FILE}
    fi
    export MY_PC_IS

    local RECIPES_DIR="${DOTFILES_DIR}/installRecipes"
    for recipe in ${RECIPES_DIR}/[0-9]*.sh; do
        if [ -x "${recipe}" ]; then
            echo
            echo ${GREEN}'*********************'${RESET}
            "${recipe}"
        fi
    done

    popd >/dev/null
    echo "Setting ${MY_PC_IS} environment finished. Please re-login"
}

doWork
