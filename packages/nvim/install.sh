#!/usr/bin/env bash

doWork() {
    local cdir=$(dirname $(readlink -f $0))

    source ${cdir}/../ensure_env.sh

    ln -sf ${cdir}/config/nvim ~/.config
    mkdir -p ~/.zsh/volatile/helpers
    ln -sf ${cdir}/nvim.zsh ~/.zsh/volatile/helpers/nvim.zsh

    echo 'Add nvim to secure path of sudo'
    sudo ln -sf ~/.zsh/volatile/igorepst/_gh_release/neovim/neovim/bin/nvim /usr/local/bin/nvim
}

doWork "$@"
