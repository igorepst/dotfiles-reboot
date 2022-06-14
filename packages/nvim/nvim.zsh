function _ig_update_nvim() {
    echo 'Updating nvim'
    if get_gh_release --repo neovim/neovim --arch linux64.tar.gz --toPath bin/nvim --tag nightly; then
	echo 'Updating nvim plugins'
        local packerDir=~/.local/share/nvim/site/pack/packer/start/packer.nvim
        [ ! -d "${packerDir}" ] && git clone --depth 1 https://github.com/wbthomason/packer.nvim "${packerDir}" 
        nvim --headless -u NONE \
            +'autocmd User PackerComplete quitall' \
            +'lua dofile(os.getenv("HOME") .. "/.config/nvim/plugin/10-options.lua")' \
            +'lua dofile(os.getenv("HOME") .. "/.config/nvim/plugin/40-pluginList.lua")' \
            +'lua require("packer").sync()'
        printf '\nUpdating Treesitter\n'
        nvim --headless -c 'TSUpdateSync' -c 'quitall'
    fi
}
_ig_update_funcs+=("_ig_update_nvim")

alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias :q='exit'

export MANPAGER='nvim +Man!'
export MANWIDTH=999
