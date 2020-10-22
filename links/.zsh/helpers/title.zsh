# Based on 
# https://github.com/sorin-ionescu/prezto/blob/master/modules/terminal/init.zsh
# and on
# https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/termsupport.zsh

if [[ "$TERM" == (tmux*|dumb|linux|*bsd*|eterm*) ]]; then
    return 1
fi

function title {
    emulate -L zsh
    setopt prompt_subst

  # if $2 is unset use $1 as default
  : ${2=$1}

  case "$TERM" in
      xterm-kitty)
          print -Pn "\e]2;$1:q\a" # set window name
          ;;
      cygwin|xterm*|putty*|rxvt*|ansi)
          print -Pn "\e]2;$2:q\a" # set window name
          print -Pn "\e]1;$1:q\a" # set tab name
          ;;
      screen*|tmux*)
          print -Pn "\ek$1:q\e\\" # set screen hardstatus
          ;;
      *)
          # Try to use terminfo to set the title
          if [[ -n "$terminfo[fsl]" ]] && [[ -n "$terminfo[tsl]" ]]; then
              echoti tsl
              print -Pn "$1"
              echoti fsl
          fi
          ;;
  esac
}

function omz_termsupport_precmd {
    emulate -L zsh
    setopt EXTENDED_GLOB
    local -a match mbegin mend
    local abbreviated_path="${PWD/#$HOME/~}"
    if [[ $abbreviated_path = (#b)(/#[^/]#/#)(*) ]]; then
        local truncated_path="${match[1]}${match[2]/(#m)?(#c15,)/...${MATCH[-12,-1]}}"
        unset MATCH
    fi
    title "$truncated_path" "$abbreviated_path"
}

# Runs before executing the command
function omz_termsupport_preexec {
    emulate -L zsh
    setopt extended_glob

  # cmd name only, or if this is sudo or ssh, the next cmd
  #local CMD=${1[(wr)^(*=*|sudo|ssh|mosh|rake|-*)]:gs/%/%%}
  #    local LINE="${1[(wr)^(*=*|ssh)]:gs/%/%%}"
  # echo "Line is $LINE"
  local cmd="${${2[(wr)^(*=*|sudo|time|noglob|-*)]}:t}"
  local truncated_cmd="${cmd/(#m)?(#c15,)/${MATCH[1,12]}...}"
  unset MATCH

  #title '$CMD' '%100>...>$LINE%<<'
  title "${truncated_cmd}" "${cmd}"
}

precmd_functions+=(omz_termsupport_precmd)
preexec_functions+=(omz_termsupport_preexec)
