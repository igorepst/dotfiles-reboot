#!/usr/bin/env zsh

function trim_path() {
    emulate -L zsh
    setopt EXTENDED_GLOB
    local -a match mbegin mend
    local abbreviated_path="${1/#$HOME/~}"
    if [[ ${abbreviated_path} = (#b)(/#[^/]#/#)(*) ]]; then
        unset MATCH
        abbreviated_path=$(basename "${abbreviated_path}")
        local len=${#abbreviated_path}
        [ $len -gt 25 ] && abbreviated_path="${abbreviated_path[0,10]}...${abbreviated_path[-12,-1]}"
    fi
    echo $abbreviated_path
}

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
        if echo ${cmd} | grep -i git; then
            cmd=git
        else
            unameHost=$(echo ${cmd} | grep -oP '(?<=-t )[^ ]*')
            [ -n "${unameHost}" ] && cmd="ssh ${unameHost#*@}"
        fi
        ;;
    *)
        cmd=$(echo ${cmd} | grep "${curCommand}" | head -n 1)
        case "${cmd}" in
            *vim*|*sh\ *|sudo*)
                showPath=1
                ;;
            *sh|*gitstatusd*)
                cmd=
                ;;
        esac
        ;;
esac
res="${cmd/#${curCommand}/$(basename ${curCommand})}"
if [ ${showPath} -eq 0 ]; then
    [ -n "${res}" ] && res="${res} "
    res=${res}$(trim_path "${curPath}")
fi
echo ${res}
