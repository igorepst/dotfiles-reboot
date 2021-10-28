#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Completing installation'${RESET}
    echo
    zsh -i -c "_updateDots && echo 'Rebuilding bat\'s cache' && bat cache --build"

    local ZSH_PLUGINS="${HOME}"/.zsh/plugins
    ln -sf "${ZSH_PLUGINS}"/archive/archive "${HOME}"/bin/archive
    chmod +x "${HOME}"/bin/archive
    ln -sf "${ZSH_PLUGINS}"/archive/lsarchive "${HOME}"/bin/lsarchive
    chmod +x "${HOME}"/bin/lsarchive
    ln -sf "${ZSH_PLUGINS}"/archive/unarchive "${HOME}"/bin/unarchive
    chmod +x "${HOME}"/bin/unarchive

    local ZSH_VOLATILE="${HOME}"/.zsh/volatile
    mkdir -p "${ZSH_VOLATILE}"
    if [[ -f "${HOME}"/.zsh_history ]] && [[ ! -f "${ZSH_VOLATILE}"/zsh_history ]]; then
        # Moving the file is problematic if called from existing ZSH session
        cp "${HOME}"/.zsh_history "${ZSH_VOLATILE}"/zsh_history
    fi
    local PMFILE="${ZSH_VOLATILE}"/pathmarks
    [ -f "${HOME}"/.pathmarks ] && mv "${HOME}"/.pathmarks "${PMFILE}"
    if [ ! -f "${PMFILE}" ]; then
        echo 'dotfiles:' ${DOTFILES_DIR}>"${PMFILE}"
        [[ "${MY_PC_IS}" = "home" ]] && echo 'inner: /mnt/Inner'>>"${PMFILE}"
    fi
}

doWork
