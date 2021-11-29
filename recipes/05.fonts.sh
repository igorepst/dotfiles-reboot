#!/usr/bin/env bash

function doWork(){
    printf "${GREEN}Installing fonts:${RESET}\n\n"

    local FONTS_DIR=${HOME}/.local/share/fonts
    mkdir -p "${FONTS_DIR}"
    declare -A FARR=( 
    ["DejaVu Sans Mono Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Regular/complete/DejaVu%20Sans%20Mono%20Nerd%20Font%20Complete%20Mono.ttf"
    ["DejaVu Sans Mono Bold Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Bold/complete/DejaVu%20Sans%20Mono%20Bold%20Nerd%20Font%20Complete%20Mono.ttf"
    ["DejaVu Sans Mono Oblique Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Italic/complete/DejaVu%20Sans%20Mono%20Oblique%20Nerd%20Font%20Complete%20Mono.ttf"
    ["DejaVu Sans Mono Bold Oblique Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Bold-Italic/complete/DejaVu%20Sans%20Mono%20Bold%20Oblique%20Nerd%20Font%20Complete%20Mono.ttf"
    ["VSCode Codicons"]="https://github.com/microsoft/vscode-codicons/raw/main/dist/codicon.ttf"
    ["Nonicons"]="https://github.com/yamatsum/nonicons/raw/master/dist/nonicons.ttf"
)

    local CONV_NEEDED=0
    for FNAME in "${!FARR[@]}"; do
        local URL=${FARR[$FNAME]}
        if [ ! -f "${FONTS_DIR}/${FNAME}.ttf" ] || [ -n "${DOT_UPDATE_FONTS}" ]; then
            CONV_NEEDED=1
            printf "${GREEN}Installing ${FNAME}.ttf${RESET}\n"
            curl -o "${FONTS_DIR}/${FNAME}.ttf" -L -O "$URL"
        else
            printf "${GREEN}${FNAME}.ttf is installed${RESET}\n"
            [ ! -f "${FONTS_DIR}/${FNAME}.otf" ] && CONV_NEEDED=1
        fi
    done
    if [ "${CONV_NEEDED}" -eq "1" ]; then
        printf "${GREEN}Converting TTF to OTF for pango${RESET}\n"
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
        printf "${GREEN}OTF fonts exist${RESET}"
    fi

    fc-cache

    local XFONTS=${HOME}/.theme/currentTheme.Xfonts
    local ROFI_SETTINGS=${HOME}/.theme/rofiSettings.rasi
    echo '*vt100.faceName: "DejaVuSansMono Nerd Font Mono":style=Book:size=16'>"${XFONTS}"
    echo 'XTerm*vt100.boldFont: "DejaVuSansMono Nerd Font Mono":style=Bold:size=16'>>"${XFONTS}"
    cat >"${ROFI_SETTINGS}" <<"EOF"
configuration {
 font: "mono 15";
 dpi: 106;
}
EOF

    printf 'Setting Fontconfig\n'
    local FONTCONFIG_DIR=${HOME}/.config/fontconfig
    mkdir -p "${FONTCONFIG_DIR}"
    local FONTCONFIG_CONF="${FONTCONFIG_DIR}/fonts.conf"
    if [[ -f "${FONTCONFIG_CONF}" ]] && [[ ! -f "${FONTCONFIG_CONF}.bak" ]]; then
        printf "Backing up ${FONTCONFIG_CONF} as ${FONTCONFIG_CONF}.bak\n"
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
    printf "${RED}Setting ${VCONS}${RESET}\n"
    sudo sh -c "cat >${VCONS}" <<'EOF'
KEYMAP=us
FONT=ter-122n
FONT_MAP=8859-1
EOF
}

doWork
vcons
