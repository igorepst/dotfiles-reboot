#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Completing installation'${RESET}
    echo
    local ZSH_PLUGINS=~/.zsh/plugins
    ln -sf "${ZSH_PLUGINS}"/archive/archive ~/bin/archive
    chmod +x ~/bin/archive
    ln -sf "${ZSH_PLUGINS}"/archive/lsarchive ~/bin/lsarchive
    chmod +x ~/bin/lsarchive
    ln -sf "${ZSH_PLUGINS}"/archive/unarchive ~/bin/unarchive
    chmod +x ~/bin/unarchive

    local ZSH_VOLATILE=~/.zsh/volatile
    mkdir -p "${ZSH_VOLATILE}"
    [ -f ~/.zsh_history ] && mv ~/.zsh_history "${ZSH_VOLATILE}"/zsh_history
    local PMFILE="${ZSH_VOLATILE}"/pathmarks
    [ -f ~/.pathmarks ] && mv ~/.pathmarks "${PMFILE}"
    if [ ! -f "${PMFILE}" ]; then
        echo 'dotfiles:' ${DOTFILES_DIR}>"${PMFILE}"
        [[ "${MY_PC_IS}" = "home" ]] || [[ "${MY_PC_IS}" = "vm" ]] && echo 'inner: /mnt/Inner'>>"${PMFILE}"
    fi
}

doWork
