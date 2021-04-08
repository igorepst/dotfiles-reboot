#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Installing fonts:'${RESET}
    echo
    local FONTS_DIR=${HOME}/.local/share/fonts
    mkdir -p "${FONTS_DIR}"
    declare -A FARR=( ["Regular"]="DejaVu Sans Mono Nerd Font Complete Mono"
    ["Bold"]="DejaVu Sans Mono Bold Nerd Font Complete Mono" 
    ["Italic"]="DejaVu Sans Mono Oblique Nerd Font Complete Mono"
    ["Bold-Italic"]="DejaVu Sans Mono Bold Oblique Nerd Font Complete Mono" )

    local CONV_NEEDED=0
    for FTYPE in "${!FARR[@]}"; do
        local FNAME=${FARR[$FTYPE]}
        if [ ! -f "${FONTS_DIR}/${FNAME}.ttf" ] || [ -n "${DOT_UPDATE_FONTS}" ]; then
            CONV_NEEDED=1
            echo "${GREEN}Installing ${FNAME}.ttf"${RESET}
            curl -o "${FONTS_DIR}/${FNAME}.ttf" -L -O \
                https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/${FTYPE}/complete/${FNAME// /%20}.ttf
        else
            echo "${GREEN}${FNAME}.ttf is installed${RESET}"
            [ ! -f "${FONTS_DIR}/${FNAME}.otf" ] && CONV_NEEDED=1
        fi
    done
    if [ "${CONV_NEEDED}" -eq "1" ]; then
        echo ${GREEN}'Converting TTF to OTF for pango'${RESET}
        local CONV_SCRIPT=convertFonts.py
        cat >"${FONTS_DIR}/${CONV_SCRIPT}" <<"EOF"
import fontforge
import os
fonts = [f for f in os.listdir('.') if f.endswith('.ttf')]
for font in fonts:
        f = fontforge.open(font)
        f.generate(font[:-3] + 'otf')
EOF
        pushd "${FONTS_DIR}" >/dev/null
        python ./"${CONV_SCRIPT}" &>/dev/null
        rm "./${CONV_SCRIPT}"
        popd >/dev/null
    else
        echo ${GREEN}'OTF fonts exist'${RESET}
    fi

    fc-cache

    local XFONTS=${HOME}/.theme/currentTheme.Xfonts
    local ROFI_SETTINGS=${HOME}/.theme/rofiSettings.rasi
    if [ "${MY_PC_IS}" = "vm" ]; then
        echo '*vt100.faceName: "DejaVuSansMono Nerd Font Mono":style=Book:size=13'>"${XFONTS}"
        echo 'XTerm*vt100.boldFont: "DejaVuSansMono Nerd Font Mono":style=Bold:size=13'>>"${XFONTS}"
        cat >"${ROFI_SETTINGS}" <<"EOF"
configuration {
 font: "mono 13";
}
EOF
    else
        echo '*vt100.faceName: "DejaVuSansMono Nerd Font Mono":style=Book:size=16'>"${XFONTS}"
        echo 'XTerm*vt100.boldFont: "DejaVuSansMono Nerd Font Mono":style=Bold:size=16'>>"${XFONTS}"
        cat >"${ROFI_SETTINGS}" <<"EOF"
configuration {
 font: "mono 15";
}
EOF
    fi

    echo 'Setting Fontconfig'
    local FONTCONFIG_DIR=${HOME}/.config/fontconfig
    mkdir -p "${FONTCONFIG_DIR}"
    local FONTCONFIG_CONF="${FONTCONFIG_DIR}/fonts.conf"
    if [[ -f "${FONTCONFIG_CONF}" ]] && [[ ! -f "${FONTCONFIG_CONF}.bak" ]]; then
        echo "Backing up ${FONTCONFIG_CONF} as ${FONTCONFIG_CONF}.bak"
        mv "${FONTCONFIG_CONF}" "${FONTCONFIG_CONF}.bak"
    fi
    cat >"${FONTCONFIG_CONF}" <<'EOF'
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
<alias>
   <family>monospace</family>
   <prefer>
     <family>DejaVuSansMono Nerd Font Mono</family>
   </prefer>
 </alias>
</fontconfig>
EOF
}

function vcons(){
    local VCONS=/etc/vconsole.conf
    [ -f "${VCONS}" ] && return
    echo "${RED}Setting ${VCONS}${RESET}"
    sudo sh -c "cat >${VCONS}" <<'EOF'
FONT=ter-122n
FONT_MAP=8859-1
EOF
}

doWork
vcons
