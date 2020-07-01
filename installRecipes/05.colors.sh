#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Setting theme colors'${RESET}
    echo
    local COLORS_DIR=~/.colors
    mkdir -p "${COLORS_DIR}"
    curl -s https://gitlab.com/protesilaos/tempus-themes/-/raw/master/xcolors/tempus_dawn.Xcolors >"${COLORS_DIR}/tempus_dawn.Xcolors"
    curl -s https://gitlab.com/protesilaos/tempus-themes/-/raw/master/xterm/tempus_dawn.Xresources >"${COLORS_DIR}/tempus_dawn.Xresources"

    ln -sf "${COLORS_DIR}/tempus_dawn.Xcolors" "${COLORS_DIR}/currentTheme.Xcolors"
    ln -sf "${COLORS_DIR}/tempus_dawn.Xresources" "${COLORS_DIR}/currentTheme.Xresources"
}

doWork
