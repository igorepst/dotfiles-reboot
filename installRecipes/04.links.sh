#!/usr/bin/env bash

function link() {
    echo ${GREEN}'Setting links'${RESET}
    echo
    local orig_file="${DOTFILES_DIR}/links/$1"
    if [ -n "$2" ]; then
        dest_file="${HOME}/$2"
    else
        dest_file="${HOME}/$1"
    fi

    mkdir -p "$(dirname "${orig_file}")"
    mkdir -p "$(dirname "${dest_file}")"

    rm -rf "${dest_file}"
    ln -s "${orig_file}" "${dest_file}"
    echo "${dest_file} -> ${orig_file}"
}

function doWork(){
    link ".zsh"
    link ".zprofile"
    link ".zshrc"
    link ".xinitrc"
    link ".Xresources"
    link ".vim"
    link ".config/awesome"
    link ".config/bat"
    link ".config/pet"
    link ".gitconfig"
    link "bin"
}

doWork
