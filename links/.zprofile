# vim:ft=zsh

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export EDITOR=nvim
export PATH=~/bin:~/.zsh/volatile/igorepst/_gh_release/_cache/_bin:~/.local/bin:$PATH
export MYTERM=kitty
export MYFEXP=xplr
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
[ -f ~/.cache/.my_pc_is ] && source ~/.cache/.my_pc_is

#  if [[ "${MY_PC_IS}" = "vm" ]] && systemctl -q is-active graphical.target && [[ ! ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
#  	 exec startx
#  fi

