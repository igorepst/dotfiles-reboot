source ~/.zsh/helpers/get_gh_release.zsh
get_gh_release --repo knqyf263/pet --arch linux_amd64.tar.gz --toPath . --toCompletionPath misc/completions/zsh
get_gh_release --repo dandavison/delta --arch x86_64-unknown-linux-gnu.tar.gz --toPath .
get_gh_release --repo denisidoro/navi --arch x86_64-unknown-linux-musl.tar.gz --toPath .
get_gh_release --repo tstack/lnav --arch linux-64bit.zip --toPath .

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# automatically remove duplicates from these arrays
typeset -U path PATH cdpath CDPATH fpath FPATH manpath MANPATH

source ~/.zsh/helpers/p10k.zsh
source ~/.zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme

source ~/.zsh/helpers/aliases.zsh
source ~/.zsh/helpers/fzf.zsh
source ~/.zsh/helpers/pet.zsh 
source ~/.zsh/helpers/ripgrep.zsh 
source ~/.zsh/helpers/vifm.zsh 
source ~/.zsh/helpers/functions.zsh 
source ~/.zsh/helpers/title.zsh 
source ~/.zsh/helpers/formarks.zsh 
source ~/.zsh/helpers/terminal.zsh 
source ~/.zsh/helpers/command_not_found.zsh 
source ~/.zsh/helpers/lnav.zsh 

[ -d ~/.work/bin ] && path+=(~/.work/bin)
[ -f ~/.work/aliases.zsh ] && source ~/.work/aliases.zsh

export npm_config_prefix=~/.node_modules

[ -d "${npm_config_prefix}" ] && path+=("${npm_config_prefix}/bin")

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

# only slash should be considered as a word separator:
function slash-backward-kill-word () {
    local WORDCHARS="${WORDCHARS:s@/@}"
    zle backward-kill-word
}
zle -N slash-backward-kill-word
#Alt+v
bindkey 'v' slash-backward-kill-word

# Without the first one, backspace doesn't work properly when returning from normal mode
bindkey "" backward-delete-char
bindkey "" backward-kill-word
bindkey "" backward-kill-line

autoload -Uz copy-earlier-word edit-command-line
zle -N copy-earlier-word
# Alt+m
bindkey 'm' copy-earlier-word
zle -N edit-command-line
# Alt+e
bindkey 'e' edit-command-line
# Alt+.
bindkey '.' insert-last-word
# Alt+i
bindkey 'i' menu-complete

zmodload zsh/complist
# Shift+Tab
bindkey -M menuselect '[Z' reverse-menu-complete
# Accept and stay in menu (multiple choices)
bindkey -M menuselect '+' accept-and-menu-complete
# Insert
bindkey -M menuselect '[2~' accept-and-menu-complete
# Accept and complete again (for ex., subdirectory)
bindkey -M menuselect 'o' accept-and-infer-next-history

HISTFILE=~/.zsh/volatile/zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

bindkey '[A' history-beginning-search-backward
bindkey '[B' history-beginning-search-forward

# Ctrl+/ to 'suspend' the half typed command
bindkey '' push-input

fpath=($fpath ~/.zsh/plugins/archive ~/.zsh/plugins/zsh-completions/src)

setopt glob_dots extended_glob auto_cd auto_pushd pushd_ignore_dups nomatch unset rm_star_silent
setopt inc_append_history share_history extended_history hist_ignore_all_dups hist_ignore_space hist_save_no_dups
setopt long_list_jobs notify no_beep complete_in_word no_hup no_flow_control

{
    autoload -Uz compinit
    local zcd=~/.zsh/volatile/zcompdump
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

export LESS_TERMCAP_mb=$'\e[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\e[1;33m'     # begin blink
export LESS_TERMCAP_us=$'\e[01;32m'    # begin underline
export LESS_TERMCAP_me=$'\e[0m'        # reset bold/blink
export LESS_TERMCAP_se=$'\e[0m'        # reset reverse video
export LESS_TERMCAP_ue=$'\e[0m'        # reset underline
export GROFF_NO_SGR=1                  # needed in some terminals, incl. kitty, to show colors in man pages
export MANPAGER='less -s -M +Gg'       # show % in man

eval $(dircolors -b)
# activate color-completion
zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}

# match uppercase from lowercase and complete from the middle of filename
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'l:|=* r:|=*'
zstyle ':completion:*' menu select
# format on completion
zstyle ':completion:*:descriptions' format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'
# separate matches into groups
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
# complete manual by their section
zstyle ':completion:*:manuals'    separate-sections true
# host completion
  [[ -r ~/.ssh/config ]] && _ssh_config_hosts=(${${(s: :)${(ps:\t:)${${(@M)${(f)"$(<$HOME/.ssh/config)"}:#Host *}#Host }}}:#*[*?]*}) || _ssh_config_hosts=()
  [[ -r ~/.ssh/known_hosts ]] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
  [[ -r /etc/hosts ]] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
hosts=(
    $(cat /etc/hostname)
    "$_ssh_config_hosts[@]"
    "$_ssh_hosts[@]"
    "$_etc_hosts[@]"
    localhost
)
zstyle ':completion:*:hosts' hosts $hosts
# on processes completion complete all user processes
zstyle ':completion:*:processes' command 'ps -au $USER'
# Provide more processes in completion of programs like killall:
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=196"
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
source ~/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
