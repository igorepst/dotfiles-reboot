#!/usr/bin/env bash

doWork() {
    local cdir=$(dirname $(readlink -f $0))

    source "${cdir}"/../ensure_env.sh

    case "${OS_ID}" in
	ubuntu)
	    if ! command -v snap > /dev/null; then
		printf "%bSnap is not installed%b\n" "${RED}" "${RESET}"
		exit 1
	    fi
	    if ! snap info emacs > /dev/null 2>&1; then
		printf "%bInstalling Emacs%b\n" "${RED}" "${RESET}"
		sudo snap install emacs --classic
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
    
    ln -sf "${cdir}"/config/emacs ~/.config
    mkdir -p ~/.zsh/volatile/helpers
    ln -sf "${cdir}"/emacs.zsh ~/.zsh/volatile/helpers/emacs.zsh

    local idir=~/.local/share/icons
    mkdir -p ${idir}
    cp -r "${cdir}"/icons/* ${idir}

    local ddir=~/.local/share/applications
    mkdir -p ${ddir}
    case "${OS_ID}" in
	ubuntu)
	    # Override for Snap
	    for d in "${cdir}"/desktop/*; do
            cp -- "${d}" "${ddir}/emacs_$(basename ${d})"
	    done
	    ;;
	*)
	    cp "${cdir}"/desktop/* ${ddir}
	    ;;
    esac
    update-desktop-database ${ddir}
    
    local sysd=~/.config/systemd/user
    local sysdl=${sysd}/default.target.wants
    mkdir -p ${sysdl}

    local edir=~/.config/environment.d
    mkdir -p ${edir}
    ln -sf "${cdir}"/environment.d/* ${edir}

    local sf=/tmp/emacs-src.tar.gz
    local sdir=~/.cache/emacs/c-src
    curl -l -L https://github.com/emacs-mirror/emacs/archive/refs/tags/emacs-28.1.tar.gz -o ${sf}
    rm -rf ${sdir}
    mkdir -p ${sdir}
    tar -xf ${sf} -C ${sdir}
    mv ${sdir}/emacs-emacs-* ${sdir}/emacs
    rm -f ${sf}

    cp "${cdir}"/emacs.service ${sysd}
    ln -sf ${sysd}/emacs.service ${sysdl}/emacs.service
    systemctl --user daemon-reload
    
    mkdir -p ~/.zsh/volatile/autostart
    ln -sf "${cdir}"/emacs-autostart.zsh ~/.zsh/volatile/autostart/emacs-autostart.zsh
}

doWork "$@"
