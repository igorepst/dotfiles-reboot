if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

fpath=($fpath ~/.zsh/volatile/igorepst/_gh_release/_cache/_compl ~/.zsh/plugins/archive ~/.zsh/plugins/zsh-completions/src)
cdpath=($HOME)

bindkey -e

# run command line as user root via sudo:
function sudo-command-line () {
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER != sudo\ * ]]; then
        BUFFER="sudo $BUFFER"
        CURSOR=$(( CURSOR+5 ))
    fi
}
zle -N sudo-command-line
# Alt+s
bindkey 's' sudo-command-line

# only slash should be considered as a word separator:
function slash-backward-kill-word () {
    local WORDCHARS="${WORDCHARS:s@/@}"
    zle backward-kill-word
}
zle -N slash-backward-kill-word
# Alt+v
bindkey 'v' slash-backward-kill-word

# Without the first one, backspace doesn't work properly when returning from normal mode
bindkey "" backward-delete-char
bindkey "" backward-kill-line
bindkey "^ " vi-forward-word

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

# Ctrl+/ to 'suspend' the half typed command. Restores on the next fresh prompt
bindkey '' push-input

setopt glob_dots extended_glob auto_cd auto_pushd pushd_ignore_dups nomatch unset rm_star_silent
setopt inc_append_history share_history extended_history hist_ignore_all_dups hist_ignore_space hist_save_no_dups
setopt long_list_jobs notify no_beep complete_in_word no_hup no_flow_control typeset_silent

{
    autoload -Uz compinit
    local zcd=~/.zsh/volatile/zcompdump
    local zcdc="$zcd.zwc"
    # To speed up loading do this once a day:
    # https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-2894219
    # Compile the completion dump to increase startup speed, if dump is newer or doesn't exist,
    # in the background as this doesn't affect the current session.
    if [[ -f "$zcd"(N.m+1) ]]; then
        compinit -i -d "$zcd"
        { rm -f "$zcdc" && zcompile "$zcd" } &!
    else
        compinit -C -d "$zcd"
        { [[ ! -f "$zcdc" || "$zcd" -nt "$zcdc" ]] && rm -f "$zcdc" && zcompile "$zcd" } &!
    fi
}

typeset -a _ig_update_funcs
for f (~/.zsh/helpers/*.zsh) source $f
for f (~/.zsh/volatile/helpers/*.zsh) source $f
[ -f ~/.work/zshrc ] && source ~/.work/zshrc

if (( ${+commands[emacsclient]} )); then
    macsman() {
	emacsclient -nc -e "(progn (require 'man)(select-frame-set-input-focus (selected-frame))(let ((Man-notify-method 'bully)) (man \"$1\")))" >/dev/null
    }
    compdef _man macsman
    alias man=macsman
elif [ "$EDITOR" = "nvim" ]; then
   export MANPAGER='nvim +Man!'
   export MANWIDTH=999
elif [ "$EDITOR" = "vim" ]; then
   export MANPAGER='vim -M +MANPAGER -'
else
   export LESS_TERMCAP_mb=$'\e[1;31m'     # begin bold
   export LESS_TERMCAP_md=$'\e[1;33m'     # begin blink
   export LESS_TERMCAP_us=$'\e[01;32m'    # begin underline
   export LESS_TERMCAP_me=$'\e[0m'        # reset bold/blink
   export LESS_TERMCAP_se=$'\e[0m'        # reset reverse video
   export LESS_TERMCAP_ue=$'\e[0m'        # reset underline
   export GROFF_NO_SGR=1                  # needed in some terminals, incl. kitty, to show colors in man pages
   export MANPAGER='less -s -M +Gg'       # show % in man
fi

LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*\#=00;90:*.bak=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.swp=00;90:*.tmp=00;90:*.dpkg-dist=00;90:*.dpkg-old=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90:'
export LS_COLORS
# activate color-completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

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

autoload -U url-quote-magic  
zle -N self-insert url-quote-magic
remote_commands=(scp rsync)
zstyle -e :urlglobber url-other-schema \
       '[[ $remote_commands[(i)$words[1]] -le ${#remote_commands} ]] && reply=("*") || reply=(http https ftp)'

zstyle -d ':completion:*' format
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':fzf-tab:*' default-color $'\033[30m'
zstyle ':fzf-tab:*' fzf-flags "--color=fg:0,bg:15,hl:1,fg+:#ffffff,bg+:#74b1d1,hl+:1,info:6,prompt:4,pointer:5,marker:6,spinner:6,header:6"
source ~/.zsh/plugins/fzf-tab/fzf-tab.plugin.zsh

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=196"
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_MANUAL_REBIND=1
source ~/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

bindkey '[A' history-substring-search-up
bindkey '[B' history-substring-search-down

if test -n "$KITTY_INSTALLATION_DIR"; then
    export KITTY_SHELL_INTEGRATION="no-cursor no-title"
    autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
    kitty-integration
    unfunction kitty-integration
fi
source ~/.zsh/plugins/powerlevel10k/powerlevel10k.zsh-theme
source ~/.zsh/supp/p10k.zsh

