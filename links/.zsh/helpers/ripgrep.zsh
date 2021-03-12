alias rg='rg --ignore-file $HOME/.config/ripgrep/ignore'

function ri() {
    local COLORVAL
    if [ -t 1 ]; then
        # Output is tty
        COLORVAL="always"
    else
        COLORVAL="never"
    fi
    local SVAL="$1"
    shift
    rg -e ${SVAL} --color=${COLORVAL} --smart-case --follow --heading --hidden --line-number --sort-files $@
}

function rf() {
    local sel rg_pref initq
    [ -n "$1" ] && initq="$1" || initq=''
    rg_pref="rg --smart-case --follow --no-heading --line-number --color=never --trim --ignore-file ~/.config/ripgrep/ignore --hidden --sort-files"
    sel=$(FZF_DEFAULT_COMMAND="$rg_pref -e '$initq'" fzf --query "$initq" --bind "change:reload:$rg_pref -e {q} || true" --disabled --info=inline --delimiter ':' --with-nth=3.. --preview 'a=$(expr {2} - 5); [ $a -lt 0 ] && a=0;((b=$a + 15));bat --color=always --line-range=$a:$b --highlight-line {2} {1}' --preview-window up)

    if [ -n "$sel" ]; then
        local ln fn tmp
        IFS=':' read -r fn ln tmp <<<"$sel"
        vim "$fn" +"$ln"
    fi
}
