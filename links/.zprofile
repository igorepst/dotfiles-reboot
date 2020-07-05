# vim:ft=zsh
export EDITOR=vim
export PATH=~/bin:$PATH
export MYTERM=xterm
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=light --reverse"
[ -f ~/dotfiles/my_pc_is ] && source ~/dotfiles/my_pc_is
if [[ "${MY_PC_IS}" = "home" ]] && systemctl -q is-active graphical.target && [[ ! ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
	exec startx
fi
