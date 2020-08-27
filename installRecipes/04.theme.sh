#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Setting theme colors'${RESET}
    echo
    local THEME_DIR=~/.theme
    mkdir -p "${THEME_DIR}"
    local CUR_COLOR=paperColor.Xresources
    cat >"${THEME_DIR}/${CUR_COLOR}" <<"EOF"
xterm*background: #eeeeee
xterm*foreground: #444444
xterm*color0: #eeeeee
xterm*color1: #af0000
xterm*color2: #008700
xterm*color3: #5f8700
xterm*color4: #0087af
xterm*color5: #878787
xterm*color6: #005f87
xterm*color7: #444444
xterm*color8: #bcbcbc
xterm*color9: #d70000
xterm*color10: #d70087
xterm*color11: #8700af
xterm*color12: #d75f00
xterm*color13: #d75f00
xterm*color14: #005faf
xterm*color15: #005f87
*background: #eeeeee
*foreground: #444444
*color0: #eeeeee
*color1: #af0000
*color2: #008700
*color3: #5f8700
*color4: #0087af
*color5: #878787
*color6: #005f87
*color7: #444444
*color8: #bcbcbc
*color9: #d70000
*color10: #d70087
*color11: #8700af
*color12: #d75f00
*color13: #d75f00
*color14: #005faf
*color15: #005f87
EOF
    ln -sf "${THEME_DIR}/${CUR_COLOR}" "${THEME_DIR}/currentTheme.Xresources"
}

doWork
