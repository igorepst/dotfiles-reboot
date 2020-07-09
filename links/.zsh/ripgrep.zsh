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
