function cd() {
    if (( ${#argv} == 1 )) && [[ -f ${1} ]]; then
        [[ ! -e ${1:h} ]] && return 1
        builtin cd ${1:h}
    else
        builtin cd "$@"
    fi
}

function cdl() {
    cd "$@" && ls -al
}

function mkcd () {
    if (( ARGC != 1 )); then
        printf 'usage: mkcd <new-directory>\n'
        return 1;
    fi
    if [[ ! -d "$1" ]]; then
        command mkdir -p "$1"
    else
        printf '`%s'\'' already exists: cd-ing.\n' "$1"
    fi
    builtin cd "$1"
}

function any () {
    emulate -L zsh
    unsetopt KSH_ARRAYS
    if [[ -z "$1" ]] ; then
        echo "any - grep for process(es) by keyword" >&2
        echo "Usage: any <keyword>" >&2 ; return 1
    else
        ps xauwww | grep -i --color=always "[${1[1]}]${1[2,-1]}"
    fi
}

function up(){
    local declare dirs=()
    get_parent_dirs() {
        if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
        if [[ "${1}" == '/' ]]; then
            for _dir in "${dirs[@]}"; do echo $_dir; done
        else
            get_parent_dirs $(dirname "$1")
        fi
    }
local DIR=$(get_parent_dirs $(realpath "${1:-$PWD}") | fzf-tmux --tac)
cd "$DIR"
}

function ssh(){
    TERM=xterm-256color command ssh -C -t $@ 'bash -l'
}

function updateDots(){
    emulate zsh; setopt localoptions
    printf '%s\n' 'Updating Git code and submodules'
    setopt no_pushd_ignore_dups 
    pushd ~/dotfiles-reboot >/dev/null
    git pull origin $(git rev-parse --abbrev-ref HEAD)
    git submodule update --recursive --remote
    popd >/dev/null
    printf '%s\n' 'Updating nvim plugins'
    nvim --headless -c 'autocmd User PackerComplete quitall' -c 'TSUpdate' -c 'PackerSync'
    printf '\n%s\n' 'Updating GH releases'
    source ${(%):-%x}
    _get_gh_releases
    rm -f ~/.zsh/volatile/zcompdump*
    exec zsh
}

function _get_gh_releases() {
    IG_GH_REL_UPDATE=1
    source ~/.zsh/helpers/get_gh_release.zsh
    get_gh_release --repo knqyf263/pet --arch linux_amd64.tar.gz --toPath pet --toCompletionPath misc/completions/zsh/_pet
    get_gh_release --repo denisidoro/navi --arch x86_64-unknown-linux-musl.tar.gz --toPath navi
    get_gh_release --repo tstack/lnav --arch linux-64bit.zip --toPath lnav
    get_gh_release --repo koalaman/shellcheck --arch linux.x86_64.tar.xz --toPath shellcheck
    get_gh_release --repo mvdan/sh --arch linux_amd64 --toPath shfmt --unarchive 0 --rn shfmt
    get_gh_release --repo JohnnyMorganz/StyLua --arch linux.zip --toPath stylua
    get_gh_release --repo lunaryorn/mdcat4 --arch x86_64-unknown-linux-musl.tar.gz --toPath mdcat
    get_gh_release --repo neovim/neovim --arch linux64.tar.gz --toPath bin/nvim --tag nightly
}
