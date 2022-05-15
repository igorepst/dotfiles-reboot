#!/usr/bin/env bash

doWork() {

    OS_ID=$(sed -ne 's/^ID=\(.*\)/\1/p' /etc/os-release)
    
    local cdir=$(dirname $(readlink -f $0))
    local idir=~/.local/share/icons
    mkdir -p ${idir}
    cp -r ${cdir}/icons/* ${idir}

    local ddir=~/.local/share/applications
    mkdir -p ${ddir}
    for dfile in ${cdir}/desktop/*.desktop; do
	case "${OS_ID}" in
	    ubuntu)
		cp ${dfile} ${ddir}/emacs_$(basename ${dfile})
		;;
	    *)
		cp ${dfile} ${ddir}
		;;
	esac
    done
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
