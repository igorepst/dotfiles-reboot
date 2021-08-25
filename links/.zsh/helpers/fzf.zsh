source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh

export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --reverse --color=fg:#282828,bg:#fffffa,hl:1,fg+:#ffffff,bg+:#74b1d1,hl+:1,info:6,prompt:4,pointer:5,marker:6,spinner:6,header:6"

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
