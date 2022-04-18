workd_prefix="${HOME}/.zsh/volatile/igorepst/_gh_release"
workd_cache="${workd_prefix}/_cache"

# returns 0 if the install/update was done
# 1 - in case of failure
# 2 - application exists and no force update was requested
# 3 - application exists, force update was requested, but there is no new version published 
function get_gh_release() {
    emulate -L zsh
    setopt no_autopushd
    zmodload zsh/zutil
    zparseopts -A _GET_GH_REL_ARGS -repo: -arch: -update:: -toPath:: -toCompletionPath:: -tag:: -unarchive:: -rn:: -rnc::
    local workd=${workd_prefix}/${_GET_GH_REL_ARGS[--repo]}
    if [ -d ${workd} ] && [ -z "${IG_GH_REL_UPDATE}" ] && [ "${_GET_GH_REL_ARGS[--update]}" != "1" ]; then
        _set_path "${workd}" "${_GET_GH_REL_ARGS[--toPath]}" "${_GET_GH_REL_ARGS[--toCompletionPath]}" \
            "${_GET_GH_REL_ARGS[--rn]}" "${_GET_GH_REL_ARGS[--rnc]}"
        return 2
    fi
    print "\033[32mFetching ${_GET_GH_REL_ARGS[--arch]} from ${_GET_GH_REL_ARGS[--repo]}\033[0m"
    local release_postfix=latest
    local tag_name=${_GET_GH_REL_ARGS[--tag]}
    [ -n "${tag_name}" ] && release_postfix=tags/${tag_name}
    local curlo=$(curl -is https://api.github.com/repos/${_GET_GH_REL_ARGS[--repo]}/releases/${release_postfix})
    [ -z ${curlo} ] && print "Empty output received" && return 1
    local code=$(echo ${curlo} |  grep -Po 'HTTP/2 \K(\d+)')
    if [ "${code}" = '301' ]; then
        curlo=$(curl -is $(echo ${curlo} |  grep -Po '"url": "\K.*?(?=")'))
        code=$(echo ${curlo} |  grep -Po 'HTTP/2 \K(\d+)')
    fi
    [ "${code}" != '200' ] && print "Received code: ${code}" && echo $curlo >>/tmp/res && return 1
    local new_ver=$(echo ${curlo} | grep -Po '"tag_name": "\K.*?(?=")')
    local new_published_at=$(echo ${curlo} | grep -Po '"published_at": "\K.*?(?=")')
    local cur_version_dir="${workd_cache}/${_GET_GH_REL_ARGS[--repo]}"
    local cur_version_file="${cur_version_dir}/version.txt"
    if [ -r "${cur_version_file}" ]; then
        zmodload zsh/mapfile
        local cur_version_array=("${(f)mapfile[${cur_version_file}]}")
        local cur_ver="${cur_version_array[1]}"
        local cur_published_at="${cur_version_array[2]}"
        print "Current version = '${cur_ver}', published at ${cur_published_at}"
        if [ "${cur_published_at}" = "${new_published_at}" ]; then
            _set_path "${workd}" "${_GET_GH_REL_ARGS[--toPath]}" "${_GET_GH_REL_ARGS[--toCompletionPath]}" \
                "${_GET_GH_REL_ARGS[--rn]}" "${_GET_GH_REL_ARGS[--rnc]}"
            return 3
        fi
    else
        print "Current version does not exist"
    fi
    local binf=$(echo ${curlo} | grep -Po '"browser_download_url": "\K.*'${(q)_GET_GH_REL_ARGS[--arch]}'?(?=")')
    [ -z ${binf} ] && print "\033[31mCannot find requested architecture\033[0m" && return 1
    print "New version     = '${new_ver}', published at ${new_published_at}"
    local tmpd=$(mktemp -d)
    pushd ${tmpd} >/dev/null
    local workf="bin${_GET_GH_REL_ARGS[--arch]}"
    curl -k -L ${binf} -o ${workf}
    [ $? -ne 0 ] && print "\033[31mCannot download requested file. URL: ${binf}\033[0m" && return 1
    rm -rf ${workd}
    mkdir -p ${workd}
    if [ -z "${_GET_GH_REL_ARGS[--unarchive]}" ] || [ "${_GET_GH_REL_ARGS[--unarchive]}" = "1" ]; then
        ! ~/.zsh/plugins/archive/unarchive ${workf} >/dev/null && print "\033[31mCannot extract the file: ${workf}\033[0m" && return 1
        rm -f ${workf}
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
    _set_path "${workd}" "${_GET_GH_REL_ARGS[--toPath]}" "${_GET_GH_REL_ARGS[--toCompletionPath]}" \
        "${_GET_GH_REL_ARGS[--rn]}" "${_GET_GH_REL_ARGS[--rnc]}"
    mkdir -p "${cur_version_dir}"
    >"${cur_version_file}" echo "${new_ver}"
    >>"${cur_version_file}" echo "${new_published_at}"
    return 0
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
            if [ -n "${4}" ]; then
                ln -sf "${top}" "${bind}/${4}"
            else
                ln -sf "${top}" "${bind}"
            fi
        fi
    fi
    if [ -n "${3}" ]; then
        local tfop="${1}/${3}"
        if [ -f "${tfop}" ]; then
            local cind="${workd_cache}/_compl"
            mkdir -p "${cind}"
            if [ -n "${5}" ]; then
                ln -sf "${tfop}" "${cind}/${5}"
            else
                ln -sf "${tfop}" "${cind}"
            fi
        fi
    fi
}
