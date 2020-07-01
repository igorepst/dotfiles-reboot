#!/usr/bin/env bash

function doWork(){
    local tmpdir=$(mktemp -d) 
    echo ${GREEN}'Installing fonts:'${RESET}
    echo
    echo ${GREEN}'Installing DejaVu Sans Mono Nerd Font Complete Mono.ttf'${RESET}
    curl -o "${tmpdir}/DejaVu Sans Mono Nerd Font Complete Mono.ttf" -L -O https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Regular/complete/DejaVu%20Sans%20Mono%20Nerd%20Font%20Complete%20Mono.ttf
    echo ${GREEN}'Installing DejaVu Sans Mono Bold Nerd Font Complete Mono.ttf'${RESET}
    curl -o "${tmpdir}/DejaVu Sans Mono Bold Nerd Font Complete Mono.ttf" -L -O https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Bold/complete/DejaVu%20Sans%20Mono%20Bold%20Nerd%20Font%20Complete%20Mono.ttf

    echo ${GREEN}'Converting TTF to OTF for pango'${RESET}
    local convertScript=convertFonts.py
    cat >"${tmpdir}/${convertScript}" <<"EOF"
import fontforge
import os
fonts = [f for f in os.listdir('.') if f.endswith('.ttf')]
for font in fonts:
        f = fontforge.open(font)
        f.generate(font[:-3] + 'otf')
EOF
    pushd "${tmpdir}" >/dev/null
    python ./"${convertScript}" &>/dev/null
    popd >/dev/null
    local fdir=~/.local/share/fonts
    mkdir -p "${fdir}"
    mv -f -t "${fdir}" "${tmpdir}/"*.[to]tf
    rm -rf "${tmpdir}"
    fc-cache

    local vcons=/etc/vconsole.conf
    if [ ! -f "${vcons}" ]; then
        echo "${RED}Setting ${vcons}${RESET}"
        sudo sh -c "cat >"${vcons}" <<"EOF"
FONT=ter-122n
FONT_MAP=8859-1
EOF"
    fi
}

doWork
