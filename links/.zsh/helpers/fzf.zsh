source ~/.zsh/volatile/igorepst/_gh_release/junegunn/fzf/completion.zsh
source ~/.zsh/volatile/igorepst/_gh_release/junegunn/fzf/key-bindings.zsh

source ~/.theme/fzfColors
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --reverse --color=$FZF_COLORS"

function f.(){
    local f
    f=$(fd --hidden -t f . ~/dotfiles-reboot | fzf)
    [ -z "$f" ] && return 1
    print -s "$EDITOR $f" # Append to history
    kitty @launch --no-response --type=tab $EDITOR "$f"
}

function kitty_list_hints(){
    local com
    com=$(sed -ne 's/^map \(kitty_mod+p[a-z+>]\+\) \(.*\)/\2 (\1)/p' ~/.config/kitty/kitty.conf | fzf --nth ..-2 --exact | awk '{$NF=""}1')
    [ -n "${com}" ] && kitty @ $(echo "${com}")
}
zle -N kitty_list_hints
bindkey 'h' kitty_list_hints
