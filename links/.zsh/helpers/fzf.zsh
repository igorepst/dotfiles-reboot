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
