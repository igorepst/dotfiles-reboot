#!/usr/bin/env bash

function doWork(){
    printf "${GREEN}Completing installation${RESET}\n\n"

    local ZSH_VOLATILE="${HOME}"/.zsh/volatile
    mkdir -p "${ZSH_VOLATILE}"
    zsh -i -c "_updateDots"

    if [[ -f "${HOME}"/.zsh_history ]] && [[ ! -f "${ZSH_VOLATILE}"/zsh_history ]]; then
        # Moving the file is problematic if called from existing ZSH session
        cp "${HOME}"/.zsh_history "${ZSH_VOLATILE}"/zsh_history
    fi
    local PMFILE="${ZSH_VOLATILE}"/pathmarks
    [ -f "${HOME}"/.pathmarks ] && mv "${HOME}"/.pathmarks "${PMFILE}"
    if [ ! -f "${PMFILE}" ]; then
        echo "dotfiles: ${DOTFILES_DIR}">"${PMFILE}"
        [[ "${MY_PC_IS}" = "home" ]] && echo 'inner: /mnt/Inner'>>"${PMFILE}"
    fi
}

doWork
