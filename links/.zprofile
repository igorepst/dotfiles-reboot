# vim:ft=zsh
export EDITOR=vim
export PATH=~/bin:$PATH
export MYTERM=xterm
export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --color=light --reverse"
if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
	exec startx
fi
