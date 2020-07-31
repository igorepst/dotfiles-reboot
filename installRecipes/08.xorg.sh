#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Setting Xorg'${RESET}
    echo
    local XORG_CONF=/etc/X11/xorg.conf.d/99-custom.conf
    if [[ ! -f "${XORG_CONF}" ]]; then
        echo "${RED}Setting ${XORG_CONF}${RESET}"
        sudo sh -c "cat >${XORG_CONF}" <<'EOF'
Section "InputClass"
    Identifier "touchpad"
    Driver "libinput"
    MatchIsTouchpad "on"
    Option "ClickMethod" "clickfinger"
    Option "NaturalScrolling" "true"
EndSection
EOF
    fi
}

doWork
