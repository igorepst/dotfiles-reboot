#!/bin/bash

dotfiles_dir="$(
    cd "$(dirname "$0")"
    pwd
)"
cd "$dotfiles_dir"

link() {
    orig_file="$dotfiles_dir/$1"
    if [ -n "$2" ]; then
        dest_file="$HOME/$2"
    else
        dest_file="$HOME/$1"
    fi

    mkdir -p "$(dirname "$orig_file")"
    mkdir -p "$(dirname "$dest_file")"

    rm -rf "$dest_file"
    ln -s "$orig_file" "$dest_file"
    echo "$dest_file -> $orig_file"
}

link ".zsh"
link ".zprofile"
link ".zshrc"
link ".xinitrc"
link ".Xresources"
link ".vim"
link ".config/awesome"
link ".local/share/fonts"
link ".gitconfig"

fc-cache

