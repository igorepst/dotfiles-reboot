export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
# ZSH doesn't do word splitting by default, so 'emacsclient -t' won't work
export EDITOR=emacsclient
export npm_config_prefix=~/.node_modules
[ -d "${npm_config_prefix}/bin" ] && PATH="${npm_config_prefix}/bin":$PATH
PATH=~/bin:~/.zsh/volatile/igorepst/_gh_release/_cache/_bin:~/.local/bin:~/.cargo/bin:~/go/bin:$PATH
[ -d ~/.work/bin ] && PATH=~/.work/bin:$PATH
export PATH
export MYTERM=kitty
export MYFEXP=vifm
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
[ -f ~/.cache/.my_pc_is ] && source ~/.cache/.my_pc_is
