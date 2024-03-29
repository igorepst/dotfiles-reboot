source ~/.zsh/volatile/igorepst/_gh_release/junegunn/fzf/completion.zsh 2>/dev/null
source ~/.zsh/volatile/igorepst/_gh_release/junegunn/fzf/key-bindings.zsh 2>/dev/null

source ~/.theme/fzfColors
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --reverse --color=$FZF_COLORS"

function f.(){
    local f
    f=$(fd --hidden -t f . ~/dotfiles-reboot | fzf)
    [ -z "$f" ] && return 1
    print -s "$EDITOR $f" # Append to history
    kitty @launch --no-response --type=tab $EDITOR "$f"
}

function _kitty_list_thing(){
    local com
    com=$(sed -ne 's/^map \(kitty_mod+'$1'[0-9a-z+>]\+\)\s\+\(.*\)/\2 (\1)/p' ~/.config/kitty/kitty.conf | fzf --nth ..-2 --exact | awk '{$NF=""}1')
    [ -n "${com}" ] && kitty @ $(echo "${com}")
}

function kitty_list_win(){
    _kitty_list_thing 'w'
}
zle -N kitty_list_win
bindkey 'w' kitty_list_win

function kitty_list_hints(){
    _kitty_list_thing 'p'
}
zle -N kitty_list_hints
bindkey 'h' kitty_list_hints

function _ig_update_fzf() {
    if get_gh_release --repo junegunn/fzf --arch linux_amd64.tar.gz --toPath fzf; then
        echo 'Downloading FZF scripts'
        local fzf_dir=~/.zsh/volatile/igorepst/_gh_release/junegunn/fzf
        curl -k -L https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh -o "${fzf_dir}"/completion.zsh
        curl -k -L https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh -o "${fzf_dir}"/key-bindings.zsh
        zcompile-many "${fzf_dir}"/*.zsh
        curl -k -L https://raw.githubusercontent.com/junegunn/fzf/master/bin/fzf-tmux -o "${fzf_dir}"/fzf-tmux
        chmod +x "${fzf_dir}"/fzf-tmux
        ln -sf "${fzf_dir}"/fzf-tmux ~/.zsh/volatile/igorepst/_gh_release/_cache/_bin/fzf-tmux
    fi
}
_ig_update_funcs+=("_ig_update_fzf")
