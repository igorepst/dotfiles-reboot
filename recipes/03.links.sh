#!/usr/bin/env bash

function link() {
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
    echo ${GREEN}'Setting links'${RESET}
    echo
    if [[ -z "${DOTFILES_DIR}" ]]; then
	echo ${RED}'Dotfiles directory is not defined. Aborting...'${RESET}
	exit 1
    fi
    link ".zsh"
    link ".zprofile"
    link ".zshrc"
    link ".xinitrc"
    link ".xprofile"
    link ".Xresources"
    link ".config/bat"
    bat cache --build 
    link ".config/pet"
    link ".config/mpv"
    link ".config/ripgrep"
    link ".config/vifm"
    link ".config/rofi"
    link ".config/nvim"
    link ".config/kitty"
    link ".config/navi"
    link ".config/picom"
    link ".config/tridactyl"
    link ".gitconfig"
    link ".gitignore-global"
    link "bin"
    link ".toprc"
    link ".config/awesome"
}

doWork
