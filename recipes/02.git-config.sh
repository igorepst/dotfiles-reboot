#!/usr/bin/env bash

function doWork() {
    echo ${GREEN}'Checking private Git configuration'${RESET}
    echo
    local gcp=~/.gitconfig-private
    if [ ! -f "${gcp}" ]; then
        read -p "Git username? " gituser
        read -p "Git email? " gitemail
        echo '[User]' >"${gcp}"
        echo 'name =' ${gituser}>>"${gcp}"
        echo 'email =' ${gitemail}>>"${gcp}"
        chmod 600 "${gcp}"
    fi

    echo ${GREEN}'Cloning submodules'${RESET}
    echo
    git submodule init
    git submodule update
    git submodule foreach 'git fetch origin; git checkout $(git rev-parse --abbrev-ref HEAD); git submodule update --recursive;'

}

doWork
