#!/usr/bin/env bash

function doWork(){
    printf "${GREEN}Setting theme colors${RESET}\n\n"

    ~/bin/setTheme
    cp ${DOTFILES_DIR}/theme/background.png ~/.theme
}

doWork
