# vim:ft=zsh

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export EDITOR=vim
export PATH=~/bin:$PATH
export MYTERM=xterm
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=light --reverse"
[ -f ~/.cache/.my_pc_is ] && source ~/.cache/.my_pc_is

if [[ "${MY_PC_IS}" = "home" ]] || [[ "${MY_PC_IS}" = "vm" ]] && systemctl -q is-active graphical.target && [[ ! ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
	exec startx
fi
