#!/usr/bin/env bash

_INST_UB_ARR=(gcc make autoconf automake pkg-config flex bison libglib2.0-bin libpango1.0-dev libpangocairo-1.0-0 libcairo2-dev libglib2.0-dev libgdk-pixbuf2.0-dev libstartup-notification0-dev libxkbcommon-dev libxkbcommon-x11-dev libxcb1-dev libxcb-xkb-dev libxcb-randr0-dev libxcb-xinerama0-dev libxcb-util-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-cursor-dev)
_INST_SRC_DIR=$HOME/github/rofi
_INST_SRC_URL=https://github.com/davatorium/rofi.git
_INST_DEST_DIR=davatorium/rofi
_INST_SRC_BRANCH=next
_INST_SRC_CONF_PARAMS=--disable-check

_inst() {
    local inst_dir="${1}"
    local bin_dir="${2}"
    killall -9 rofi
    cp -r rofi "${inst_dir}"
    ln -sf "${inst_dir}/rofi" "${bin_dir}/rofi"
}

doWork() {
    local cdir=$(dirname $(readlink -f $0))

    source ${cdir}/../ensure_env.sh
    source ${cdir}/../build_common.sh

    if _build; then
	ln -sf ${cdir}/config/rofi ~/.config
	exec zsh
    fi
}

doWork "$@"
