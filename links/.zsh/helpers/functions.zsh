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
    #    echo 'Updating nvim plugins'
    #    nvim --headless +PackerUpdate +TSUpdate +qall
    echo 'Updating Git submodules'
    setopt no_pushd_ignore_dups 
    pushd ~/dotfiles-reboot >/dev/null
    git pull origin $(git rev-parse --abbrev-ref HEAD)
    git submodule update --recursive --remote
    popd >/dev/null
    echo 'Updating GH releases'
    IG_GH_REL_UPDATE=1
    source ~/.zsh/helpers/get_gh_release.zsh
    get_gh_release --repo knqyf263/pet --arch linux_amd64.tar.gz --toPath pet --toCompletionPath misc/completions/zsh/_pet
    get_gh_release --repo dandavison/delta --arch x86_64-unknown-linux-gnu.tar.gz --toPath delta
    get_gh_release --repo denisidoro/navi --arch x86_64-unknown-linux-musl.tar.gz --toPath navi
    get_gh_release --repo tstack/lnav --arch linux-64bit.zip --toPath lnav
    get_gh_release --repo neovim/neovim --arch linux64.tar.gz --toPath bin/nvim --tag nightly
    rm -f ~/.zsh/volatile/zcompdump*
    exec zsh
}
