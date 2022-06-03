#!/usr/bin/env bash

function _build() {
    local arr
    printf "${GREEN}Checking OS${RESET}\n"
    case "${OS_ID}" in
        arch) arr=( "${_INST_ARCH_ARR[@]}" ) ;;
        ubuntu) arr=( "${_INST_UB_ARR[@]}" ) ;;
        *)
            printf "${RED}Unsupported OS: ${OS_ID}${RESET}\n"
            exit 1
            ;;
    esac
    local pnames=""
    printf "${GREEN}Checking prerequisites packages to install:${RESET}\n\n"
    for i in "${arr[@]}"; do
        checkp "${OS_ID}" "${i}" && printf "${GREEN}*${RESET} $i ${GREEN}is installed${RESET}\n" || pnames="${pnames} $i"
    done
    if [ -n "${pnames}" ]; then
        printf "${RED}Installing the following packages:${RESET}\n${pnames}\n"
        case "${OS_ID}" in
            arch) sudo /usr/bin/pacman -Sy --noconfirm ${pnames} ;;
            ubuntu) sudo /usr/bin/apt-get update && sudo /usr/bin/apt-get install -y ${pnames} ;;
        esac
    else
        printf "${GREEN}All packages are installed${RESET}\n"
    fi

    printf "${GREEN}Updating Git code${RESET}\n"
    if [ -d "${_INST_SRC_DIR}" ]; then
        pushd "${_INST_SRC_DIR}" > /dev/null || exit 1
        git reset --hard
        if ! git pull origin ${_INST_SRC_BRANCH}; then
            printf "${RED}Git pull failed${RESET}\n"
            popd > /dev/null || exit 1
            exit 1
        fi
        if ! git submodule update --init; then
            printf "${RED}Git submodule update failed${RESET}\n"
            popd > /dev/null || exit 1
            exit 1
        fi
    else
        mkdir -p "${_INST_SRC_DIR}"
        if ! git clone --recursive "${_INST_SRC_URL}" "${_INST_SRC_DIR}"; then
            printf "${RED}Git clone failed${RESET}\n"
            popd > /dev/null || exit 1
            exit 1
        fi
        pushd "${_INST_SRC_DIR}" > /dev/null || exit 1
    fi

    printf "${GREEN}Starting configure${RESET}\n"
    CFLAGS+=' -fcommon'
    [ ! -f ./configure ] && autoreconf -i
    if ! ./configure ${_INST_SRC_CONF_PARAMS}; then
        printf "${RED}Configure failed${RESET}\n"
        popd > /dev/null || exit 1
        exit 1
    fi
    printf "${GREEN}Starting make${RESET}\n"
    if ! make; then
        printf "${RED}Make failed${RESET}\n"
        popd > /dev/null || exit 1
        exit 1
    fi

    local inst_dir=~/.zsh/volatile/igorepst/_gh_release/${_INST_DEST_DIR}
    local bin_dir=~/.zsh/volatile/igorepst/_gh_release/_cache/_bin
    local compl_dir=~/.zsh/volatile/igorepst/_gh_release/_cache/_compl
    mkdir -p "${inst_dir}"
    _inst "${inst_dir}" "${bin_dir}" "${compl_dir}"
    rm -f ~/.zsh/volatile/zcompdump* 2> /dev/null

    popd > /dev/null || exit 1
    printf "${GREEN}Done successfully${RESET}\n"
}
