# http://zsh.sourceforge.net/Doc/Release/Command-Execution.html
# https://github.com/falconindy/pkgfile/blob/master/extra/command-not-found.zsh
function command_not_found_handler() {
    local cmd="$1"
    if [ -x /usr/bin/pacman ]; then
        local pkgs
        pkgs=(${(f)"$(pacman -F "$cmd" 2>/dev/null)"})
        if [[ -n "$pkgs" ]]; then
            printf '%s is not installed, but may be found in the following packages:\n' "$cmd"
            printf '  %s\n' $pkgs[@]
            return 127
        fi
    elif [ -x /usr/lib/command-not-found ]; then
        /usr/lib/command-not-found -- "$cmd"
        return 127
    elif [ -x /usr/share/command-not-found/command-not-found ]; then
        /usr/share/command-not-found/command-not-found -- "$cmd"
        return 127
    fi
    printf "zsh: command not found: ${cmd}\n"
    return 127
}
