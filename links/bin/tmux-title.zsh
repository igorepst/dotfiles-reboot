#!/usr/bin/env zsh

curCommand=$1
curPath=$2
paneTty=$3
cmd=$(ps --no-headers -t ${paneTty} -o args --sort=-start_time)
showPath=0
case "${cmd}" in
    *vifm*)
        cmd=Vifm
        ;;
    *java*Dmaven*)
        cmd=mvn
        ;;
    *ssh*)
        showPath=1
        cmd=$(echo ${cmd} | grep ssh | head -n 1)
        unameHost=$(echo ${cmd} | grep -oP '(?<=-t )[^ ]*')
        [ -n "${unameHost}" ] && cmd="ssh ${unameHost#*@}"
        ;;
    *)
        cmd=$(echo ${cmd} | grep "${curCommand}" | head -n 1)
        case "${cmd}" in
            *vim*|*sh\ *)
                showPath=1
                ;;
            *sh|*gitstatusd*)
                cmd=
                ;;
        esac
        ;;
esac
res=${cmd}
if [ ${showPath} -eq 0 ]; then
    [ -n "${res}" ] && res="${res} "
    res=${res}$(basename "${curPath}")
fi
echo ${res}
