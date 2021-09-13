alias rg='rg --ignore-file $HOME/.config/ripgrep/ignore'

function ri() {
    kitty +kitten hyperlinked_grep --ignore-file $HOME/.config/ripgrep/ignore --smart-case --follow --hidden --sort-files "$@"
}
compdef _rg ri

function rif() {
    local res ec
    res=$(rg --ignore-file $HOME/.config/ripgrep/ignore --files-with-matches --smart-case --follow --hidden --sort-files "$@")
    ec=$?
    if [ ${ec} -eq 0 ]; then
        local hn=$(hostname)
        local f
        while read f; do
            echo "]8;;file://${hn}$(readlink -f ${f})${f}]8;;" 
        done<<<"${res}"
    fi
    return ${ec}
}
compdef _rg rif

function rf() {
    local sel rg_pref initq
    [ -n "$1" ] && initq="$1" || initq=''
    rg_pref="rg --smart-case --follow --no-heading --line-number --color=never --trim --ignore-file ~/.config/ripgrep/ignore --hidden --sort-files"
    sel=$(FZF_DEFAULT_COMMAND="$rg_pref -e '$initq'" fzf --query "$initq" --bind "change:reload:$rg_pref -e {q} || true" --disabled --info=inline --delimiter ':' --with-nth=3.. --preview 'a=$(expr {2} - 5);[ $a -lt 0 ] && a=0;((b=$a + 15));bat --color=always --line-range=$a:$b --highlight-line {2} {1}' --preview-window up)

    if [ -n "$sel" ]; then
        local ln fn tmp
        IFS=':' read -r fn ln tmp <<<"$sel"
        # Add to history
        print -s $EDITOR "$fn" +"$ln"
        $EDITOR "$fn" +"$ln"
    fi
}
