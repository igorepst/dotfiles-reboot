#!/usr/bin/env bash

{
    DOTFILES_DIR="$(
    cd "$(dirname "$0")"
    pwd
    )"
    export DOTFILES_DIR
    pushd "$DOTFILES_DIR" >/dev/null

    local MY_PC_ANSWER_FILE="$DOTFILES_DIR/my_pc_is"
    [ -f $MY_PC_ANSWER_FILE ] && source $MY_PC_ANSWER_FILE
    if [ -z $MY_PC_IS ]; then
        local MY_PC_IS_H
        read -p "Is it a home PC? [Y]es:[N]o " MY_PC_IS_H
        if [[ "$MY_PC_IS_H" =~ ^[Yy]$ ]]; then
            MY_PC_IS="home"
        else
            MY_PC_IS="work"	
        fi
        echo 'export MY_PC_IS='$MY_PC_IS>$MY_PC_ANSWER_FILE
    fi
    export MY_PC_IS


    local recipes_dir="$DOTFILES_DIR/installRecipes"
    "$recipes_dir/01.packages.sh"
    "$recipes_dir/02.gitconfig-private.sh"
    "$recipes_dir/03.fonts.sh"
    "$recipes_dir/04.links.sh"
    "$recipes_dir/99.else.sh"

    popd >/dev/null
}
