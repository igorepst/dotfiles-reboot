#!/usr/bin/env bash

function doWork(){
    local TMP_DIR=$(mktemp -d) 
    echo ${GREEN}'Installing fonts:'${RESET}
    echo
    local FONTS_DIR=~/.local/share/fonts
    local REG_FONT="DejaVu Sans Mono Nerd Font Complete Mono"
    local CONV_NEEDED=0
    if [ ! -f "${FONTS_DIR}/${REG_FONT}.ttf" ] || [ -n "${DOT_UPDATE_FONTS}" ]; then 
        CONV_NEEDED=1
        echo "${GREEN}Installing ${REG_FONT}.ttf"${RESET}
        curl -o "${TMP_DIR}/${REG_FONT}.ttf" -L -O \
https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Regular/complete/DejaVu%20Sans%20Mono%20Nerd%20Font%20Complete%20Mono.ttf
    else
        echo "${GREEN}${REG_FONT}.ttf is installed${RESET}"
    fi
    local BOLD_FONT="DejaVu Sans Mono Bold Nerd Font Complete Mono"
    if [ ! -f "${FONTS_DIR}/${BOLD_FONT}.ttf" ] || [ -n "${DOT_UPDATE_FONTS}" ]; then 
        CONV_NEEDED=1
        echo "${GREEN}Installing ${BOLD_FONT}.ttf"${RESET}
        curl -o "${TMP_DIR}/${BOLD_FONT}.ttf" -L -O \
https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Bold/complete/DejaVu%20Sans%20Mono%20Bold%20Nerd%20Font%20Complete%20Mono.ttf
    else
        echo "${GREEN}${BOLD_FONT}.ttf is installed${RESET}"
    fi

    if [ "${CONV_NEEDED}" -eq "1" ] || [ ! -f "${FONTS_DIR}/${REG_FONT}.otf" ] || [ ! -f "${FONTS_DIR}/${BOLD_FONT}.otf" ]; then 
        echo ${GREEN}'Converting TTF to OTF for pango'${RESET}
        local CONV_SCRIPT=convertFonts.py
        cat >"${TMP_DIR}/${CONV_SCRIPT}" <<"EOF"
import fontforge
import os
fonts = [f for f in os.listdir('.') if f.endswith('.ttf')]
for font in fonts:
        f = fontforge.open(font)
        f.generate(font[:-3] + 'otf')
EOF
        pushd "${TMP_DIR}" >/dev/null
        python ./"${CONV_SCRIPT}" &>/dev/null
        popd >/dev/null
    else
        echo ${GREEN}'OTF fonts exist'${RESET}
    fi

    mkdir -p "${FONTS_DIR}"
    mv -f -t "${FONTS_DIR}" "${TMP_DIR}/"*.[to]tf 2>/dev/null
    rm -rf "${TMP_DIR}"
    fc-cache

    local XFONTS=${HOME}/.theme/currentTheme.Xfonts
    if [ "${MY_PC_IS}" = "vm" ]; then
        echo '*vt100.faceName: "DejaVuSansMono Nerd Font Mono":style=Book:size=13'>"${XFONTS}"
        echo 'XTerm*vt100.boldFont: "DejaVuSansMono Nerd Font Mono":style=Bold:size=13'>>"${XFONTS}"
    else
        echo '*vt100.faceName: "DejaVuSansMono Nerd Font Mono":style=Book:size=16'>"${XFONTS}"
        echo 'XTerm*vt100.boldFont: "DejaVuSansMono Nerd Font Mono":style=Bold:size=16'>>"${XFONTS}"
    fi
}

function vcons(){
    local VCONS=/etc/vconsole.conf
    [ -f "${VCONS}" ] && return
    echo "${RED}Setting ${VCONS}${RESET}"
    sudo sh -c "cat >"${VCONS}" <<"EOF"
FONT=ter-122n
FONT_MAP=8859-1
EOF"
}

doWork
vcons
