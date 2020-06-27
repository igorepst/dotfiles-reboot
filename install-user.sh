#!/bin/bash

dotfiles_dir="$(
    cd "$(dirname "$0")"
    pwd
)"
cd "$dotfiles_dir"

[ -f ./my_pc_is ] && source ./my_pc_is
if [ -z $MY_PC_IS ]; then
	read -p "Is it a home PC? [Y]es:[N]o " MY_PC_IS_H
	if [[ "$MY_PC_IS_H" =~ ^[Yy]$ ]]; then
  		MY_PC_IS="home"
	else
      		MY_PC_IS="work"	
	fi
	echo 'export MY_PC_IS='$MY_PC_IS>my_pc_is
fi


recipes_dir="$dotfiles_dir/installRecipes"
"$recipes_dir/01.packages.sh"
"$recipes_dir/02.gitconfig-private.sh"

link() {
    orig_file="$dotfiles_dir/links/$1"
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
link ".config/bat"
link ".config/pet"
link ".local/share/fonts"
link ".gitconfig"
link "bin"

mkdir -p ~/.zsh/volatile 


fc-cache

ln -sf ~/.zsh/plugins/archive/archive ~/bin/archive
chmod +x ~/bin/archive
ln -sf ~/.zsh/plugins/archive/lsarchive ~/bin/lsarchive
chmod +x ~/bin/lsarchive
ln -sf ~/.zsh/plugins/archive/unarchive ~/bin/unarchive
chmod +x ~/bin/unarchive

pmfile=~/.zsh/volatile/pathmarks
if [ ! -f "$pmfile" ]; then
	echo 'dotfiles:' $dotfiles_dir>"$pmfile"
	[[ "$MY_PC_IS" = "home" ]] && echo 'inner: /mnt/Inner'>>"$pmfile"
fi
