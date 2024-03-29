#!/usr/bin/env bash

function doWorkKDE(){
    printf "${GREEN}Installing KDE settings${RESET}\n"

    printf "Processing shortcuts\n"
    local SH_RC="$HOME/.config/kglobalshortcutsrc"
    kwriteconfig5 --file "${SH_RC}" --group "krunner.desktop" --key "RunClipboard" "none,none,Run command on clipboard contents"
    kwriteconfig5 --file "${SH_RC}" --group "krunner.desktop" --key "_launch" "none,none,KRunner"
    kwriteconfig5 --file "${SH_RC}" --group "plasmashell" --key "show dashboard" "none,none,Show Desktop"
    kwriteconfig5 --file "${SH_RC}" --group "ksmserver" --key "Lock Session" "Meta+L\tScreensaver,Meta+L\tScreensaver,Lock Session"
    # Workaround for kwriteconfig escaping \t as \\t. Without quotes, \t is escaped as only t.
    sed -i 's@\\\\t@\\t@g' "${SH_RC}"

    # The name 'kglobalaccel' should have '5' only in the second case
    [ "$TERM" != "linux" ] && kquitapp5 kglobalaccel && sleep 2s && kglobalaccel5 &

    printf "Disabling Baloo indexing\n"
    kwriteconfig5 --file baloofilerc --group 'Basic Settings' --key 'Indexing-Enabled' false

    if [ -n "${DOTFILES_DIR}" ]; then
        printf "Setting Xresources for KDE\n"
        # KDE overrides various aspects
        local SRC_XPROF="${HOME}/.config/autostart-scripts/srcXprofile"
        cat >"${SRC_XPROF}" <<"EOF"
#!/bin/sh
source ~/.xprofile
EOF
        chmod +x "${SRC_XPROF}"
    fi
}

function doWorkGnome() {
    printf "${GREEN}Installing Gnome settings${RESET}\n\n"

    while read line
do
	( [[ -z "${line}" ]] || [[ ${line} =~ ^#.* ]] ) && continue
	read -r schema key value <<< "${line}"
	gsettings set ${schema} ${key} "${value}"
done <<EOM

org.gnome.desktop.interface enable-animations false
org.gnome.desktop.interface enable-hot-corners false
org.gnome.desktop.interface show-battery-percentage true
org.gnome.desktop.interface buttons-have-icons true
org.gnome.desktop.interface menus-have-icons true
org.gnome.desktop.interface clock-format '24h'
org.gnome.desktop.interface clock-show-date true
org.gnome.desktop.interface clock-show-weekday true
org.gnome.desktop.datetime automatic-timezone true
org.gnome.desktop.calendar show-weekdate true

org.gnome.desktop.wm.preferences audible-bell false
org.gnome.desktop.wm.preferences visual-bell false
org.gnome.desktop.wm.preferences focus-mode 'mouse'

org.gnome.desktop.peripherals.touchpad click-method 'fingers'
org.gnome.desktop.peripherals.touchpad natural-scroll true
org.gnome.desktop.input-sources xkb-options ['grp_led:scroll','grp:caps_toggle']
org.gnome.mutter overlay-key 'Super_R'
org.gnome.mutter dynamic-workspaces false
org.gnome.desktop.wm.preferences num-workspaces 1
org.gnome.shell.keybindings switch-to-application-1 ['']
org.gnome.shell.keybindings switch-to-application-2 ['']
org.gnome.shell.keybindings switch-to-application-3 ['']
org.gnome.shell.keybindings switch-to-application-4 ['']
org.gnome.shell.keybindings switch-to-application-5 ['']
org.gnome.shell.keybindings switch-to-application-6 ['']
org.gnome.shell.keybindings switch-to-application-7 ['']
org.gnome.shell.keybindings switch-to-application-8 ['']
org.gnome.shell.keybindings switch-to-application-9 ['']
org.gnome.shell.keybindings focus-active-notification ['']
org.gnome.shell.keybindings open-application-menu ['']
org.gnome.shell.keybindings toggle-application-view ['']
org.gnome.shell.keybindings toggle-message-tray ['']
org.gnome.shell.keybindings toggle-overview ['']
org.gnome.desktop.wm.keybindings show-desktop ['<Super>d']
org.gnome.desktop.wm.keybindings switch-to-workspace-up []
org.gnome.desktop.wm.keybindings switch-to-workspace-down []
org.gnome.desktop.wm.keybindings activate-window-menu []

org.gtk.Settings.FileChooser show-hidden true
org.gnome.nautilus.preferences default-folder-viewer 'list-view'
org.gnome.nautilus.preferences always-use-location-entry true
org.gnome.nautilus.preferences show-create-link true
org.gnome.nautilus.preferences show-delete-permanently true
org.gnome.nautilus.list-view default-visible-columns ['name','size','owner','group','permissions','date_modified','detailed_type']

org.gnome.shell.extensions.desktop-icons show-trash true
org.gnome.shell.extensions.desktop-icons icon-size 'small'
org.gnome.shell.extensions.desktop-icons show-home true

# Dash-to-panel [gnome-shell-extension-dash-to-panel]
org.gnome.shell.extensions.dash-to-panel activate-single-window true
org.gnome.shell.extensions.dash-to-panel animate-app-switch false
org.gnome.shell.extensions.dash-to-panel animate-show-apps false
org.gnome.shell.extensions.dash-to-panel animate-window-launch false
org.gnome.shell.extensions.dash-to-panel check-update false
org.gnome.shell.extensions.dash-to-panel click-action 'CYCLE-MIN'
org.gnome.shell.extensions.dash-to-panel customize-click true
org.gnome.shell.extensions.dash-to-panel force-check-update false
org.gnome.shell.extensions.dash-to-panel group-apps false
org.gnome.shell.extensions.dash-to-panel group-apps-underline-unfocused true
org.gnome.shell.extensions.dash-to-panel group-apps-use-fixed-width true
org.gnome.shell.extensions.dash-to-panel group-apps-use-launchers true
org.gnome.shell.extensions.dash-to-panel hot-keys false
org.gnome.shell.extensions.dash-to-panel intellihide false
org.gnome.shell.extensions.dash-to-panel isolate-monitors false
org.gnome.shell.extensions.dash-to-panel isolate-workspaces true
org.gnome.shell.extensions.dash-to-panel location-clock 'STATUSRIGHT'
org.gnome.shell.extensions.dash-to-panel middle-click-action 'LAUNCH'
org.gnome.shell.extensions.dash-to-panel minimize-shift true
org.gnome.shell.extensions.dash-to-panel multi-monitors true
org.gnome.shell.extensions.dash-to-panel panel-position 'BOTTOM'
org.gnome.shell.extensions.dash-to-panel peek-mode true
org.gnome.shell.extensions.dash-to-panel primary-monitor -1
org.gnome.shell.extensions.dash-to-panel progress-show-bar true
org.gnome.shell.extensions.dash-to-panel progress-show-count true
org.gnome.shell.extensions.dash-to-panel scroll-icon-action 'NOTHING'
org.gnome.shell.extensions.dash-to-panel scroll-panel-action 'NOTHING'
org.gnome.shell.extensions.dash-to-panel secondarymenu-contains-appmenu true
org.gnome.shell.extensions.dash-to-panel secondarymenu-contains-showdetails true
org.gnome.shell.extensions.dash-to-panel shift-click-action 'MINIMIZE'
org.gnome.shell.extensions.dash-to-panel shift-middle-click-action 'LAUNCH'
org.gnome.shell.extensions.dash-to-panel shortcut-previews false
org.gnome.shell.extensions.dash-to-panel show-activities-button false
org.gnome.shell.extensions.dash-to-panel show-appmenu false
org.gnome.shell.extensions.dash-to-panel show-apps-icon-file ''
org.gnome.shell.extensions.dash-to-panel show-clock-all-monitors true
org.gnome.shell.extensions.dash-to-panel show-favorites-all-monitors true
org.gnome.shell.extensions.dash-to-panel show-favorites false
org.gnome.shell.extensions.dash-to-panel show-running-apps true
org.gnome.shell.extensions.dash-to-panel show-show-apps-button true
org.gnome.shell.extensions.dash-to-panel show-showdesktop-button false
org.gnome.shell.extensions.dash-to-panel show-status-menu-all-monitors true
org.gnome.shell.extensions.dash-to-panel show-tooltip true
org.gnome.shell.extensions.dash-to-panel show-window-previews false
org.gnome.shell.extensions.dash-to-panel stockgs-force-hotcorner false
org.gnome.shell.extensions.dash-to-panel stockgs-keep-dash false
org.gnome.shell.extensions.dash-to-panel stockgs-panelbtn-click-only false
org.gnome.shell.extensions.dash-to-panel taskbar-locked false
org.gnome.shell.extensions.dash-to-panel taskbar-position 'LEFTPANEL'
org.gnome.shell.extensions.dash-to-panel version-to-install ''

org.gnome.shell.app-switcher current-workspace-only true
org.gnome.shell.window-switcher current-workspace-only true
org.gnome.shell disabled-extensions ['ubuntu-dock@ubuntu.com']
org.gnome.shell enabled-extensions ['dash-to-panel@jderose9.github.com']
org.gnome.shell remember-mount-password true

org.gnome.Terminal.Legacy.Settings tab-position 'bottom'
org.gnome.Terminal.Legacy.Settings confirm-close false 
org.gnome.Terminal.Legacy.Settings default-show-menubar false
org.gnome.Terminal.Legacy.Settings tab-policy 'always'
org.gnome.Terminal.Legacy.Settings theme-variant 'light'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ visible-name 'Mine'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ audible-bell false
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ background-color 'rgb(238,238,236)'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ cursor-background-color 'rgb(252,151,30)'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ cursor-blink-mode 'off'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ cursor-colors-set true
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ font 'Monospace 16'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ foreground-color 'rgb(46,52,54)'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ highlight-background-color 'rgb(116,177,209)'
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ highlight-colors-set true
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ palette ['rgb(46,52,54)','rgb(204,0,0)','rgb(78,154,6)','rgb(196,160,0)','rgb(52,101,164)','rgb(117,80,123)','rgb(6,152,154)','rgb(211,215,207)','rgb(85,87,83)','rgb(239,41,41)','rgb(138,226,52)','rgb(252,233,79)','rgb(114,159,207)','rgb(173,127,168)','rgb(52,226,226)','rgb(238,238,236)']
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ use-system-font false
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ use-theme-colors false
org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9/ use-theme-transparency false

org.gnome.gnome-system-monitor.proctree col-14-visible true
org.gnome.gnome-system-monitor.proctree col-2-visible true
org.gnome.gnome-system-monitor.proctree columns-order [0,1,2,3,4,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,14]

org.gnome.desktop.default-applications.terminal exec 'kitty'
org.gnome.desktop.default-applications.terminal exec-arg '-e'

org.gnome.settings-daemon.plugins.media-keys terminal ['<Super>Return']
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ binding '<Super>r'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ command 'rofiLauncher'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ name 'Rofi'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/ binding '<Super>e'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/ command 'tfm'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/ name 'TerminalFM'
org.gnome.settings-daemon.plugins.media-keys screenshot []
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/ binding 'Print'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/ command 'gnome-screenshot -ia'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/ name 'Gnome Screenshot'
# Ctrl+Shift+Esc
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/ binding '<Primary><Shift>Escape'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/ command 'gnome-system-monitor'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/ name 'Gnome System Monitor'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/ binding '<Super>a'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/ command 'visual'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/ name 'Visual editor'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5/ binding '<Super>u'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5/ command 'emacsReopenFrame'
org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5/ name 'Reopen Emacs frame'
org.gnome.settings-daemon.plugins.media-keys custom-keybindings ['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/','/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/','/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/','/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/','/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom4/','/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom5/']

EOM
}

if command -v kwriteconfig5 &> /dev/null; then
    doWorkKDE
else
    printf "${GREEN}Skipping KDE configuration${RESET}\n\n"
fi

if command -v gsettings &> /dev/null; then
    doWorkGnome
else
    printf "${GREEN}Skipping Gnome configuration${RESET}\n\n"
fi

