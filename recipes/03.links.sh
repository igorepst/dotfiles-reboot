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
    printf "${GREEN}Setting links${RESET}\n\n"

    if [[ -z "${DOTFILES_DIR}" ]]; then
	printf "${RED}Dotfiles directory is not defined. Aborting...${RESET}"
	exit 1
    fi
    link ".zsh"
    link ".zshenv"
    link ".zshrc"
    link ".zprofile"
    ln -sf ~/.zprofile ~/.profile
    link ".xinitrc"
    link ".xprofile"
    link ".Xresources"
    link ".config/bat"
    link ".config/mpv"
    link ".config/ripgrep"
    link ".config/vifm"
    link ".config/rofi"
    link ".config/nvim"
    link ".config/kitty"
    link ".config/navi"
    link ".config/picom"
    link ".config/tridactyl"
    link ".config/mediainfo"
    link ".gitconfig"
    link ".gitignore-global"
    link "bin"
    link ".toprc"
    link ".config/awesome"
    link ".config/emacs"
}

doWork
