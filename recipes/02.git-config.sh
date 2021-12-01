#!/usr/bin/env bash

function doWork() {
    printf "${GREEN}Checking private Git configuration${RESET}\n\n"

    local gcp=~/.gitconfig-private
    if [ ! -f "${gcp}" ]; then
        read -rp "Git username? " gituser
        read -rp "Git email? " gitemail
        {
            echo '[User]'
            echo "name = ${gituser}"
            echo "email = ${gitemail}"
        }>"${gcp}"
    chmod 600 "${gcp}"
    fi

    printf "${GREEN}Cloning submodules${RESET}\n\n"

    git submodule init
    git submodule update

}

doWork
