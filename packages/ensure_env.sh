#!/usr/bin/env bash

export RED='\033[0;31m'
export GREEN='\033[0;32m'
export RESET='\033[0m'
if [ -z "${OS_ID}" ]; then
    OS_ID=$(sed -ne 's/^ID=\(.*\)/\1/p' /etc/os-release)
    export OS_ID
fi

function checkp() {
    case "${OS_ID}" in
        arch) /usr/bin/pacman -Qs '^'"${1}"'$' > /dev/null ;;
        ubuntu) /usr/bin/dpkg -s "${1}" > /dev/null 2>&1 ;;
    esac
}
