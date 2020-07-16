#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Setting theme colors'${RESET}
    echo
    local THEME_DIR=~/.theme
    mkdir -p "${THEME_DIR}"
    curl -s https://gitlab.com/protesilaos/tempus-themes/-/raw/master/xcolors/tempus_dawn.Xcolors >"${THEME_DIR}/tempus_dawn.Xcolors"
    curl -s https://gitlab.com/protesilaos/tempus-themes/-/raw/master/xterm/tempus_dawn.Xresources >"${THEME_DIR}/tempus_dawn.Xresources"

    ln -sf "${THEME_DIR}/tempus_dawn.Xcolors" "${THEME_DIR}/currentTheme.Xcolors"
    ln -sf "${THEME_DIR}/tempus_dawn.Xresources" "${THEME_DIR}/currentTheme.Xresources"
}

doWork
