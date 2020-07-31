#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Installing KDE settings'${RESET}
    echo

    echo Processing shortcuts
    local SH_RC="$HOME/.config/kglobalshortcutsrc"
    kwriteconfig5 --file "${SH_RC}" --group "krunner.desktop" --key "RunClipboard" "none,none,Run command on clipboard contents"
    kwriteconfig5 --file "${SH_RC}" --group "krunner.desktop" --key "_launch" "none,none,KRunner"
    kwriteconfig5 --file "${SH_RC}" --group "plasmashell" --key "show dashboard" "none,none,Show Desktop"
    kwriteconfig5 --file "${SH_RC}" --group "ksmserver" --key "Lock Session" "Meta+L\tScreensaver,Meta+L\tScreensaver,Lock Session"
    # Workaround for kwriteconfig escaping \t as \\t. Without quotes, \t is escaped as only t.
    sed -i 's@\\\\t@\\t@g' "${SH_RC}"

    # The name 'kglobalaccel' should have '5' only in the second case
    [ "$TERM" != "linux" ] && kquitapp5 kglobalaccel && sleep 2s && kglobalaccel5 &

    echo Disabling Baloo indexing
    kwriteconfig5 --file baloofilerc --group 'Basic Settings' --key 'Indexing-Enabled' false

    if [ -n "${DOTFILES_DIR}" ]; then
        echo Setting Xresources for KDE
        # KDE overrides various aspects
        local SRC_XPROF="${HOME}/.config/autostart-scripts/srcXprofile"
        cat >"${SRC_XPROF}" <<"EOF"
#!/bin/sh
source ~/.xprofile
EOF
        chmod +x "${SRC_XPROF}"
    fi
}

if command -v kwriteconfig5 &> /dev/null; then
    doWork
else
    echo ${GREEN}'Skipping KDE configuration'${RESET}
    echo
fi
