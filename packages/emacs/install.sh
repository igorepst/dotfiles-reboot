#!/usr/bin/env bash

doWork() {
    local cdir=$(dirname $(readlink -f $0))

    source ${cdir}/../ensure_env.sh

    case "${OS_ID}" in
	ubuntu)
	    if ! checkp emacs; then
		printf "%bInstalling Emacs%b\n" "${RED}" "${RESET}"
		sudo add-apt-repository ppa:kelleyk/emacs
		sudo apt-get update
		sudo apt install emacs28
	    fi
	    ;;
	arch)
	    if ! checkp emacs-nativecomp; then
		printf "%bInstalling Emacs%b\n" "${RED}" "${RESET}"
		sudo /usr/bin/pacman -Sy --noconfirm emacs-nativecomp
	    fi
	    ;;
	*)
	    printf "%bUnsupported OS: %s%b\n" "${RED}" "${OS_ID}" "${RESET}"
	    exit 1
	    ;;
    esac
    
    ln -sf ${cdir}/config/emacs ~/.config

    local idir=~/.local/share/icons
    mkdir -p ${idir}
    cp -r ${cdir}/icons/* ${idir}

    local ddir=~/.local/share/applications
    mkdir -p ${ddir}
    cp ${cdir}/desktop/* ${ddir}
    update-desktop-database ${ddir}
    
    local sysd=~/.config/systemd/user
    local sysdl=${sysd}/default.target.wants
    mkdir -p ${sysdl}

    local edir=~/.config/environment.d
    mkdir -p ${edir}
    ln -sf ${cdir}/environment.d/* ${edir}

    cp ${cdir}/emacs.service ${sysd}
    ln -sf ${sysd}/emacs.service ${sysdl}/emacs.service
    systemctl --user daemon-reload
    systemctl --user enable --now emacs.service
}

doWork "$@"
