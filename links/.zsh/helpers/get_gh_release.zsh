workd_prefix="${HOME}/.zsh/volatile/igorepst/_gh_release"
workd_cache="${workd_prefix}/_cache"

function get_gh_release() {
    emulate -L zsh
    setopt no_autopushd
    zmodload zsh/zutil
    zparseopts -A _GET_GH_REL_ARGS -repo: -arch: -update:: -toPath:: -toCompletionPath:: -tag:: -unarchive:: -rn::
    local workd=${workd_prefix}/${_GET_GH_REL_ARGS[--repo]}
    if [ -d ${workd} ] && [ -z "${IG_GH_REL_UPDATE}" ] && [ "${_GET_GH_REL_ARGS[--update]}" != "1" ]; then
        _set_path ${workd} ${_GET_GH_REL_ARGS[--toPath]} ${_GET_GH_REL_ARGS[--toCompletionPath]}
        return
    fi
    print "\033[32mFetching ${_GET_GH_REL_ARGS[--arch]} from ${_GET_GH_REL_ARGS[--repo]}\033[0m"
    local release_postfix=latest
    local tag_name=${_GET_GH_REL_ARGS[--tag]}
    [ -n "${tag_name}" ] && release_postfix=tags/${tag_name}
    local curlo=$(curl -s https://api.github.com/repos/${_GET_GH_REL_ARGS[--repo]}/releases/${release_postfix})
    [ -z ${curlo} ] && print "Empty output received" && return
    local new_ver=$(echo ${curlo} | grep -Po '"tag_name": "\K.*?(?=")')
    local new_published_at=$(echo ${curlo} | grep -Po '"published_at": "\K.*?(?=")')
    print "New version is '${new_ver}', published at ${new_published_at}"
    local cur_version_dir="${workd_cache}/${_GET_GH_REL_ARGS[--repo]}"
    local cur_version_file="${cur_version_dir}/version.txt"
    if [ -r "${cur_version_file}" ]; then
        zmodload zsh/mapfile
        local cur_version_array=("${(f)mapfile[${cur_version_file}]}")
        local cur_ver="${cur_version_array[1]}"
        local cur_published_at="${cur_version_array[2]}"
        print "Current version is '${cur_ver}', published at ${cur_published_at}"
        if [ "${cur_published_at}" = "${new_published_at}" ]; then
            _set_path ${workd} ${_GET_GH_REL_ARGS[--toPath]} ${_GET_GH_REL_ARGS[--toCompletionPath]}
            return
        fi
    else
        print "Current version doesn't exist"
    fi
    local binf=$(echo ${curlo} | grep -Po '"browser_download_url": "\K.*'${(q)_GET_GH_REL_ARGS[--arch]}'?(?=")')
    [ -z ${binf} ] && print "Cannot find requested architecture" && return
    local tmpd=$(mktemp -d)
    pushd ${tmpd} >/dev/null
    local workf
    [ -n "${_GET_GH_REL_ARGS[--rn]}" ] && workf="${_GET_GH_REL_ARGS[--rn]}" || workf="bin${_GET_GH_REL_ARGS[--arch]}"
    curl -k -L ${binf} -o ${workf}
    [ $? -ne 0 ] && print "Cannot download requested file. URL: ${binf}" && return
    rm -rf ${workd}
    mkdir -p ${workd}
    if [ -z "${_GET_GH_REL_ARGS[--unarchive]}" ] || [ "${_GET_GH_REL_ARGS[--unarchive]}" = "1" ]; then
        ~/.zsh/plugins/archive/unarchive ${workf} >/dev/null
        [ $? -ne 0 ] && print "Cannot extract the file: ${workf}" && return
        rm ${workf}
    fi
    local dir_name=$(_check_dirs ${tmpd})
    if [ -n "$dir_name" ]; then 
        local dir_name1=$(_check_dirs ${dir_name})
        [ -n "$dir_name1" ] && cd ${dir_name1} || cd ${dir_name}
    fi
    autoload -Uz zmv
    zmv '*' ${workd}
    popd >/dev/null
    rm -rf ${tmpd}
    _set_path ${workd} ${_GET_GH_REL_ARGS[--toPath]} ${_GET_GH_REL_ARGS[--toCompletionPath]}
    mkdir -p "${cur_version_dir}"
    >"${cur_version_file}" echo "${new_ver}"
    >>"${cur_version_file}" echo "${new_published_at}"
}

function _check_dirs() {
    local dir=0
    local dir_name
    for f in ${1}/*(DN); do
        [ -f ${f} ] && return
        if [ -d ${f} ]; then 
            ((dir++))
            [[ $dir -gt 1 ]] && return
            dir_name=${f}
        fi
    done
    echo $dir_name
}

function _set_path(){
    if [ -n "${2}" ]; then
        local top="${1}/${2}"
        if [ -f "${top}" ]; then
            chmod +x "${top}"
            local bind="${workd_cache}/_bin"
            mkdir -p "${bind}"
            ln -sf "${top}" "${bind}"
        fi
    fi
    if [ -n "${3}" ]; then
        local tfop="${1}/${3}"
        if [ -f "${tfop}" ]; then
            local cind="${workd_cache}/_compl"
            mkdir -p "${cind}"
            ln -sf "${tfop}" "${cind}"
        fi
    fi
}
