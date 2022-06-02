#!/usr/bin/env bash

function doWork() {
    printf "${GREEN}Completing installation${RESET}\n\n"

    npm install -g npm-check-updates
    pip install pyright

    local ZSH_VOLATILE="${HOME}"/.zsh/volatile
    mkdir -p "${ZSH_VOLATILE}"
    if [[ -f "${HOME}"/.zsh_history ]] && [[ ! -f "${ZSH_VOLATILE}"/zsh_history ]]; then
        # Moving the file is problematic if called from existing ZSH session
        cp "${HOME}"/.zsh_history "${ZSH_VOLATILE}"/zsh_history
    fi
    local PMFILE="${ZSH_VOLATILE}"/pathmarks
    [ -f "${HOME}"/.pathmarks ] && mv "${HOME}"/.pathmarks "${PMFILE}"
    if [ ! -f "${PMFILE}" ]; then
        echo "dotfiles: ${DOTFILES_DIR}" > "${PMFILE}"
        [[ "${MY_PC_IS}" = "home" ]] && echo 'inner: /mnt/Inner' >> "${PMFILE}"
    fi

    mkdir -p ~/.zsh/volatile/igorepst/_gh_release/_cache/_compl
    curl -k -L https://raw.githubusercontent.com/docker/cli/master/contrib/completion/zsh/_docker -o ~/.zsh/volatile/igorepst/_gh_release/_cache/_compl/_docker
    curl -k -L https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/plugins/pip/_pip -o ~/.zsh/volatile/igorepst/_gh_release/_cache/_compl/_pip

    # Run as login intercative shell to re-read config
    zsh -li -c "_updateDots"
}

doWork
