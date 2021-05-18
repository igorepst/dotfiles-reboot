# vim:ft=zsh

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export EDITOR=nvim
export PATH=~/bin:~/.cargo/bin:~/.cabal/bin:~/.zsh/volatile/igorepst/_gh_release/_cache/_bin:~/.local/bin:$PATH
export FPATH=~/.zsh/volatile/igorepst/_gh_release/_cache/_compl:$FPATH
export MYTERM=kitty
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
[ -f ~/.cache/.my_pc_is ] && source ~/.cache/.my_pc_is

if [[ "${MY_PC_IS}" = "vm" ]] && systemctl -q is-active graphical.target && [[ ! ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
	 exec startx
fi

