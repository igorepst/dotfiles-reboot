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
    echo 'Updating Git code and submodules'
    setopt no_pushd_ignore_dups 
    pushd ~/dotfiles-reboot >/dev/null
    git pull origin $(git rev-parse --abbrev-ref HEAD)
    git submodule update --recursive --remote --init
    popd >/dev/null
    echo 'Updating GH releases'
    source ${(%):-%x}
    _get_gh_releases
    echo 'Updating nvim plugins'
    rehash
    local packerDir=~/.local/share/nvim/site/pack/packer/start/packer.nvim
    [ ! -d "${packerDir}" ] && git clone https://github.com/wbthomason/packer.nvim "${packerDir}" 
    _install_nvim_lsp
    nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
    nvim --headless -c 'TSUpdateSync' -c 'quitall'
    rm -f ~/.zsh/volatile/zcompdump*
    exec zsh
}

function _get_gh_releases() {
    IG_GH_REL_UPDATE=1
    source ~/.zsh/helpers/get_gh_release.zsh
    get_gh_release --repo denisidoro/navi --arch x86_64-unknown-linux-musl.tar.gz --toPath navi
    get_gh_release --repo tstack/lnav --arch musl-64bit.zip --toPath lnav
    get_gh_release --repo koalaman/shellcheck --arch linux.x86_64.tar.xz --toPath shellcheck
    get_gh_release --repo mvdan/sh --arch linux_amd64 --toPath shfmt --unarchive 0 --rn shfmt
    get_gh_release --repo JohnnyMorganz/StyLua --arch linux.zip --toPath stylua
    get_gh_release --repo sayanarijit/xplr --arch linux.tar.gz --toPath xplr
    get_gh_release --repo neovim/neovim --arch linux64.tar.gz --toPath bin/nvim --tag nightly
}

function _install_nvim_lsp() {
    emulate -L zsh
    setopt no_autopushd
    local parentDir=~/.cache/nvim/lspServers
    # Bash
    if [ ! -d "${parentDir}/bash" ]; then
        echo 'Installing Bash LSP for Neovim'
        mkdir -p "${parentDir}/bash"
        pushd "${parentDir}/bash" > /dev/null
        npm install bash-language-server
        popd > /dev/null
    fi
    # Lua
    if [ ! -d "${parentDir}/lua" ]; then
        echo 'Installing Lua LSP for Neovim'
        mkdir -p "${parentDir}/lua"
        pushd "${parentDir}/lua" > /dev/null
        curl -L -o sumneko-lua.vsix $(curl -s https://api.github.com/repos/sumneko/vscode-lua/releases/latest | grep 'browser_' | cut -d\" -f4)
        rm -rf sumneko-lua
        unzip sumneko-lua.vsix -d sumneko-lua
        rm sumneko-lua.vsix
        chmod +x sumneko-lua/extension/server/bin/Linux/lua-language-server
        popd > /dev/null
    fi
}
