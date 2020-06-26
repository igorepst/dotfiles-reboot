function get_gh_release() {
    emulate -L zsh
    setopt no_autopushd
    zmodload zsh/zutil
    zparseopts -A _GET_GH_REL_ARGS -repo: -arch: -update:: -toPath:: -toCompletionPath:: -tag::
    local workd_prefix=$HOME/.zsh/volatile/igorepst/_gh_release
    local workd=${workd_prefix}/${_GET_GH_REL_ARGS[--repo]}
    if [ -d ${workd} ] && [ "${_GET_GH_REL_ARGS[--update]}" != "1" ]; then
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
    local cur_version_dir="${workd_prefix}/_cache/${_GET_GH_REL_ARGS[--repo]}"
    local cur_version_file="${cur_version_dir}/version.txt"
    if [ -r "${cur_version_file}" ]; then
        zmodload zsh/mapfile
        local cur_version_array=("${(f)mapfile[${cur_version_file}]}")
        local cur_ver="${cur_version_array[1]}"
        local cur_published_at="${cur_version_array[2]}"
        print "Current version is '${cur_ver}', published at ${cur_published_at}"
        [ "${cur_published_at}" = "${new_published_at}" ] && return
    else
        print "Current version doesn't exist"
    fi
    local binf=$(echo ${curlo} | grep -Po '"browser_download_url": "\K.*'${(q)_GET_GH_REL_ARGS[--arch]}'?(?=")')
    [ -z ${binf} ] && print "Cannot find requested architecture" && return
    local tmpd=$(mktemp -d)
    pushd ${tmpd} >/dev/null
    local workf=bin${_GET_GH_REL_ARGS[--arch]}
    curl -L ${binf} -o ${workf}
    [ $? -ne 0 ] && print "Cannot download requested file. URL: ${binf}" && return
    rm -rf ${workd}
    mkdir -p ${workd}
    tar -xvzf ${workf} >/dev/null
    rm ${workf}
    local dir_name=$(_check_dirs ${tmpd})
    [ -n "$dir_name" ] && cd ${dir_name}
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
        if [ "." = "${2}" ]; then
            path=(${1} $path)
        else
            path=(${1}/${2} $path)
        fi
    fi
    if [ -n "${3}" ]; then
        if [ "." = "${3}" ]; then
            fpath=(${1} $fpath)
        else
            fpath=(${1}/${3} $fpath)
        fi
    fi
}
