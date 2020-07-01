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

    local MY_PC_ANSWER_FILE="${DOTFILES_DIR}/my_pc_is"
    [ -f ${MY_PC_ANSWER_FILE} ] && source ${MY_PC_ANSWER_FILE}
    if [ -z ${MY_PC_IS} ]; then
        local MY_PC_IS_H
        echo
        echo ${RED}'Checking environment'${RESET}
        read -p "Is it a home PC? [Y]es:[N]o " MY_PC_IS_H
        if [[ "${MY_PC_IS_H}" =~ ^[Yy]$ ]]; then
            MY_PC_IS="home"
        else
            MY_PC_IS="work"	
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
}

doWork
