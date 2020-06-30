#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Finishing installation'${RESET}
    echo
    local zsh_plugins=~/.zsh/plugins
    ln -sf ${zsh_plugins}/archive/archive ~/bin/archive
    chmod +x ~/bin/archive
    ln -sf ${zsh_plugins}/archive/lsarchive ~/bin/lsarchive
    chmod +x ~/bin/lsarchive
    ln -sf ${zsh_plugins}/archive/unarchive ~/bin/unarchive
    chmod +x ~/bin/unarchive

    local zsh_volatile=~/.zsh/volatile
    mkdir -p {$zsh_volatile}
    [ -f ~/.zsh_history ] && mv ~/.zsh_history ${zsh_volatile}/zsh_history
    local pmfile=${zsh_volatile}/pathmarks
    if [ ! -f "${pmfile}" ]; then
        echo 'dotfiles:' ${DOTFILES_DIR}>"${pmfile}"
        [[ "${MY_PC_IS}" = "home" ]] && echo 'inner: /mnt/Inner'>>"${pmfile}"
    fi
}

doWork
