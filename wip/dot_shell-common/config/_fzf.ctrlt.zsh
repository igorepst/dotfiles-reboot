# Based on https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh

if [[ $- == *i* ]]; then

# CTRL-T - Paste the selected file path(s) into the command line
__fsel() {
  local cr=$CURSOR
  local cmd="${FZF_CTRL_T_COMMAND}"
  setopt localoptions pipefail 2> /dev/null
  local -a itemsArr; itemsArr=()
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS --expect=ctrl-o" \
	  $(__fzfcmd) -m "$@" | while read item; do itemsArr+="${item}"; done
  if [[ ${#itemsArr} == 0 ]]; then
  	echo -n 0
  	return 0
  fi
  if [[ "${itemsArr[1]}" == "ctrl-o" ]]; then
	  cr=0
	  itemsArr=(${itemsArr:1})
  fi
  if [[ $cr == 0 ]]; then
  	local pr=$(echo $'vim\ncd\nranger\nxdg-open' | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS --no-preview" $(__fzfcmd))
	if [[ -z "$pr" ]]; then
		echo -n 0
		return 0
	fi
  	echo -n "1$pr "
	if [[ "$pr" == "ranger" ]]; then
  		for item in $itemsArr; do
			echo -n "$(dirname -- ${(q)item}) "
  		done
	else
  		for item in $itemsArr; do
  			echo -n "${(q)item} "
  		done
	fi
  else	
	echo -n 0  
	for item in $itemsArr; do
		echo -n "${(q)item} "
	done
  fi

  local ret=$?
  echo
  return $ret
}

__fzf_use_tmux__() {
  [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
}

__fzfcmd() {
  __fzf_use_tmux__ &&
    echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

fzf-file-widget() {
  local lb="$(__fsel)"
  local ret=$?
  if [[ "${lb[1]}" == "1" ]]; then
	LBUFFER="${lb[2,${#lb}]}"
  else
	LBUFFER="${LBUFFER}${lb[2,${#lb}]}" 
  fi
  zle reset-prompt
  return $ret
}
zle     -N   fzf-file-widget
bindkey '^T' fzf-file-widget

fi
