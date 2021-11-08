#!/usr/bin/env bash

function doWork(){
    printf "${GREEN}Setting Xorg${RESET}\n\n"

    [[ ! -d /etc/X11/xorg.conf.d ]] && sudo mkdir -p /etc/X11/xorg.conf.d
    local XORG_CONF=/etc/X11/xorg.conf.d/99-custom.conf
    if [[ ! -f "${XORG_CONF}" ]]; then
        printf "${RED}Setting ${XORG_CONF}${RESET}\n"
        sudo sh -c "cat >${XORG_CONF}" <<'EOF'
Section "InputClass"
   Identifier "touchpad"
   Driver "libinput"
   MatchIsTouchpad "on"
   Option "Tapping" "true"
   Option "TappingDrag" "true"
   Option "TappingDragLock" "true"
   Option "AccelProfile" "adaptive"
   Option "AccelSpeed" "0.00"
   Option "DisableWhileTyping" "true"
   Option "ScrollMethod" "twofinger"
   Option "ClickMethod" "clickfinger"
   Option "NaturalScrolling" "true"
EndSection

Section "InputClass"
        Identifier "system-keyboard"
        MatchIsKeyboard "on"
        Option "XkbLayout" "us,ru,il"
        Option "XkbModel" "pc104"
        Option "XkbVariant" ",phonetic,"
        Option "XkbOptions" "grp:caps_toggle"
EndSection
EOF
    fi
}

doWork
