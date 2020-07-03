source ~/.zsh/helpers/get_gh_release.zsh
get_gh_release --repo knqyf263/pet --arch linux_amd64.tar.gz --toPath . --toCompletionPath misc/completions/zsh

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prom:pt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source ~/.zsh/p10k.zsh
source ~/.zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme

#Alt+g
export FZF_MARKS_JUMP='g'
export PATHMARKS_FILE=~/.zsh/volatile/pathmarks
source ~/.zsh/plugins/formarks/formarks.plugin.zsh

source ~/.zsh/aliases.zsh
source ~/.zsh/fzf.zsh
source ~/.zsh/pet.zsh 
source ~/.zsh/title.zsh 

# run command line as user root via sudo:
function sudo-command-line () {
[[ -z $BUFFER ]] && zle up-history
if [[ $BUFFER != sudo\ * ]]; then
    BUFFER="sudo $BUFFER"
    CURSOR=$(( CURSOR+5 ))
fi
}
zle -N sudo-command-line
#Alt+s
bindkey 's' sudo-command-line

autoload -Uz copy-earlier-word edit-command-line
zle -N copy-earlier-word
#Alt+m
bindkey 'm' copy-earlier-word
zle -N edit-command-line
#Alt+e
bindkey 'e' edit-command-line
#Alt+.
bindkey '.' insert-last-word

HISTFILE=~/.zsh/volatile/zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

bindkey '[A' history-beginning-search-backward
bindkey '[B' history-beginning-search-forward

fpath=($fpath ~/.zsh/plugins/archive ~/.zsh/plugins/zsh-completions/src)

setopt globdots extendedglob auto_cd

{
    autoload -Uz compinit
    local zcd=.zsh/volatile/zcompdump
    local zcdc="$zcd.zwc"
    # To speed up loading do this once a day:
    # https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-2894219
    # Compile the completion dump to increase startup speed, if dump is newer or doesn't exist,
    # in the background as this is doesn't affect the current session.
    if [[ -f "$zcd"(#qN.m+1) ]]; then
        compinit -i -d "$zcd"
        { rm -f "$zcdc" && zcompile "$zcd" } &!
        else
            compinit -C -d "$zcd"
            { [[ ! -f "$zcdc" || "$zcd" -nt "$zcdc" ]] && rm -f "$zcdc" && zcompile "$zcd" } &!
    fi
}

# Ctrl+S will no longer freeze the terminal
stty stop '' -ixoff -ixon

# match uppercase from lowercase and complete from the middle of filename
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
zstyle ':completion:*' menu select

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=6"
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
source ~/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
