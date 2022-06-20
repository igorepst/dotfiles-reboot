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
    local dir=$(get_parent_dirs $(realpath "${1:-$PWD}") | fzf-tmux)
     if [[ -z "$dir" ]]; then
    zle redisplay
    return 0
  fi
  zle push-line
  BUFFER="cd -- ${(q)dir}"
  zle accept-line
  local ret=$?
  unset dir
  zle reset-prompt
  return $ret
}
zle -N up
# Alt+x
bindkey 'x' up

function ssh(){
    TERM=xterm-256color command ssh -C -t $@ 'bash -l'
}

function updateDots(){
    _updateDots
    exec zsh
}

function zcompile-many() {
  local f
  for f; do zcompile -R -- "$f".zwc "$f"; done
}

function _updateDots(){
    emulate zsh; setopt localoptions
    setopt no_pushd_ignore_dups
    if [ -d ~/.work ]; then
        echo 'Updating work Git code and submodules'
        pushd ~/.work >/dev/null
        git pull origin $(git rev-parse --abbrev-ref HEAD)
        popd >/dev/null
    fi
    echo 'Updating Git code and submodules'
    pushd ~/dotfiles-reboot >/dev/null
    git pull origin $(git rev-parse --abbrev-ref HEAD)
    zcompile-many ~/dotfiles-reboot/links/.zsh/helpers/*.zsh
    zcompile-many ~/dotfiles-reboot/links/.zsh/volatile/helpers/*.zsh
    zcompile-many ~/dotfiles-reboot/links/.zsh/supp/*.zsh
    git submodule update --init --recursive --remote --merge --force
    zcompile-many ~/dotfiles-reboot/links/.zsh/plugins/zsh-syntax-highlighting/{zsh-syntax-highlighting.zsh,highlighters/*/*.zsh}
    zcompile-many ~/dotfiles-reboot/links/.zsh/plugins/zsh-autosuggestions/{zsh-autosuggestions.zsh,src/**/*.zsh}
    popd >/dev/null
    source ${(%):-%x}
    rehash
    IG_GH_REL_UPDATE=1
    source ~/.zsh/supp/get_gh_release.zsh
    for i ("$_ig_update_funcs[@]") $i
    _get_gh_releases
    ! ncu -gs -e 2 npm-check-updates && echo 'Updating npm-check-updates' && npm i -g npm-check-updates
    _install_lsp
    echo 'Updating Python packages'
    echo 'Visidata:'
    pip3 install -U visidata
    rm -f ~/.zsh/volatile/zcompdump* 2>/dev/null
}

function _get_gh_releases() {
    get_gh_release --repo koalaman/shellcheck --arch linux.x86_64.tar.xz --toPath shellcheck
    get_gh_release --repo mvdan/sh --arch linux_amd64 --toPath binlinux_amd64 --unarchive 0 --rn shfmt
    get_gh_release --repo JohnnyMorganz/StyLua --arch linux.zip --toPath stylua
    if get_gh_release --repo sharkdp/bat --arch x86_64-unknown-linux-gnu.tar.gz --toPath bat --toCompletionPath autocomplete/bat.zsh --rnc _bat; then
        rehash
        bat cache --build
    fi
    get_gh_release --repo sharkdp/fd --arch x86_64-unknown-linux-gnu.tar.gz --toPath fd --toCompletionPath autocomplete/_fd
    if get_gh_release --repo kovidgoyal/kitty --arch x86_64.txz --toPath bin/kitty; then
        mkdir -p ~/.terminfo/x
        ln -sf ~/.zsh/volatile/igorepst/_gh_release/kovidgoyal/kitty/share/terminfo/x/xterm-kitty ~/.terminfo/x/xterm-kitty
    fi
    get_gh_release --repo rclone/rclone --arch linux-amd64.zip --toPath rclone
    get_gh_release --repo rust-analyzer/rust-analyzer --arch x86_64-unknown-linux-gnu.gz --toPath binx86_64-unknown-linux-gnu --rn rust-analyzer
}

function _install_npm_lsp() {
    emulate -L zsh
    setopt no_autopushd
    local res=1
    if [ ! -d "${1}" ]; then
        echo "Installing ${2} LSP"
        mkdir -p "${1}"
        pushd "${1}" > /dev/null
        npm install ${3}
	res=$?
    else
        echo "Updating ${2}"
        pushd "${1}" > /dev/null
        ! ncu -us -e 2 && npm install
	res=$?
    fi
    popd > /dev/null
    return res
}

function _install_lsp() {
    emulate -L zsh
    setopt no_autopushd
    local parentDir=~/.cache/lspServers
    # Bash
    if _install_npm_lsp "${parentDir}/bash" 'Bash' 'bash-language-server'; then
	 mkdir -p ~/.local/bin
	 ln -sf "${parentDir}"/bash/node_modules/.bin/bash-language-server ~/.local/bin
    fi
    # Pyright
    if _install_npm_lsp "${parentDir}/pyright" 'Pyright' 'pyright'; then
	 mkdir -p ~/.local/bin
	 ln -sf "${parentDir}"/pyright/node_modules/pyright/index.js ~/.local/bin/pyright
	 ln -sf "${parentDir}"/pyright/node_modules/pyright/langserver.index.js ~/.local/bin/pyright-langserver
    fi
    # HTML/CSS/JSON/ESLint (JS/TS)
    _install_npm_lsp "${parentDir}/vscode-langservers-extracted" 'HTML/CSS/JSON/ESLint' 'vscode-langservers-extracted'
    # Dockerfile
    _install_npm_lsp "${parentDir}/dockerfile" 'Dockerfile' 'dockerfile-language-server-nodejs'
    # Lua
    local inst_lua url version
    url=$(sed -ne 's/.*browser_download_url.*"\(http.*linux-x64.vsix\)"/\1/p' <<< $(curl -s https://api.github.com/repos/sumneko/vscode-lua/releases/latest))
    version=$(sed -ne 's|.*/v\(.*\)/.*|\1|p' <<< "${url}")
    if [ ! -d "${parentDir}/lua" ]; then
        printf '\nInstalling Lua LSP, version: %s\n' "${version}"
        inst_lua=1
        mkdir -p "${parentDir}/lua"
    else
        local cur_ver
        cur_ver=$(sed -ne 's/.*version\": "\(.*\)".*/\1/p' "${parentDir}/lua/sumneko-lua/extension/package.json")
        printf '\nUpdating Lua LSP, current version: %s, new version: %s\n' "${cur_ver}" "${version}"
        [ "${cur_ver}" != "${version}" ] && inst_lua=1
    fi
    if [ -n "${inst_lua}" ]; then
        pushd "${parentDir}/lua" > /dev/null
        curl -L -o sumneko-lua.vsix "${url}"
        rm -rf sumneko-lua
        unzip -q sumneko-lua.vsix -d sumneko-lua
        rm sumneko-lua.vsix
        popd > /dev/null
    fi
    # Go
    local gopls_status
    command -v gopls >/dev/null && gopls_status='Updating' || gopls_status='Installing'
    echo "${gopls_status} Go LSP"
    go install golang.org/x/tools/gopls@latest
    rehash
    gopls version
}
