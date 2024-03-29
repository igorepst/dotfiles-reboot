#!/usr/bin/env bash

function updateFonts(){
     printf "${GREEN}Installing fonts:${RESET}\n\n"

    local FONTS_DIR=${HOME}/.local/share/fonts
    mkdir -p "${FONTS_DIR}"
    declare -A FARR=( 
    ["DejaVu Sans Mono Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Regular/complete/DejaVu%20Sans%20Mono%20Nerd%20Font%20Complete%20Mono.ttf"
    ["DejaVu Sans Mono Bold Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Bold/complete/DejaVu%20Sans%20Mono%20Bold%20Nerd%20Font%20Complete%20Mono.ttf"
    ["DejaVu Sans Mono Oblique Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Italic/complete/DejaVu%20Sans%20Mono%20Oblique%20Nerd%20Font%20Complete%20Mono.ttf"
    ["DejaVu Sans Mono Bold Oblique Nerd Font Complete Mono"]="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Bold-Italic/complete/DejaVu%20Sans%20Mono%20Bold%20Oblique%20Nerd%20Font%20Complete%20Mono.ttf"
    ["Noto Sans Symbols2"]="https://github.com/googlefonts/noto-fonts/raw/main/hinted/ttf/NotoSansSymbols2/NotoSansSymbols2-Regular.ttf"
    ["Noto Color Emoji"]="https://github.com/googlefonts/noto-emoji/raw/main/fonts/NotoColorEmoji.ttf"
)

    for FNAME in "${!FARR[@]}"; do
        local URL=${FARR[$FNAME]}
        if [ ! -f "${FONTS_DIR}/${FNAME}.ttf" ] || [ -n "${DOT_UPDATE_FONTS}" ]; then
            printf "${GREEN}Installing ${FNAME}.ttf${RESET}\n"
            curl -o "${FONTS_DIR}/${FNAME}.ttf" -L -O "$URL"
        else
            printf "${GREEN}${FNAME}.ttf is installed${RESET}\n"
        fi
    done

    fc-cache
}

function doWork(){
    updateFonts

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
