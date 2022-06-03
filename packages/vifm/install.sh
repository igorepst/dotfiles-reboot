#!/usr/bin/env bash

_INST_UB_ARR=(libncursesw5-dev libmagic-dev)
_INST_SRC_DIR=$HOME/github/vifm
_INST_SRC_URL=https://github.com/vifm/vifm.git
_INST_DEST_DIR=vifm/vifm
_INST_SRC_BRANCH=master

_inst() {
    local inst_dir="${1}"
    local bin_dir="${2}"
    local compl_dir="${3}"
    cp -r data src/vifm src/vifm-pause data/shell-completion/zsh/_vifm "${inst_dir}"
    ln -sf "${inst_dir}/vifm" "${bin_dir}/vifm"
    ln -sf "${inst_dir}/vifm-pause" "${bin_dir}/vifm-pause"
    ln -sf "${inst_dir}/_vifm" "${compl_dir}/_vifm"
}

doWork() {
    local cdir=$(dirname $(readlink -f $0))

    source ${cdir}/../ensure_env.sh
    source ${cdir}/../build_common.sh

    if _build; then
	ln -sf ${cdir}/config/vifm ~/.config
	ln -sf ${cdir}/vifm.zsh ~/.zsh/helpers/vifm.zsh
	exec zsh
    fi
}

doWork "$@"
