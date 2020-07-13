# http://zsh.sourceforge.net/Doc/Release/Command-Execution.html
# https://github.com/falconindy/pkgfile/blob/master/extra/command-not-found.zsh
command_not_found_handler() {
  local pkgs cmd="$1"

  pkgs=(${(f)"$(pacman -F "$cmd" 2>/dev/null)"})
  if [[ -n "$pkgs" ]]; then
    printf '%s is not installed, but may be found in the following packages:\n' "$cmd"
    printf '  %s\n' $pkgs[@]
  else
    printf 'zsh: command not found: %s\n' "$cmd"
  fi 1>&2

  return 127
}
