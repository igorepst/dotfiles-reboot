alias ls='ls --color=auto --hyperlink=auto'
alias la='ls -alh'
alias ll='ls -alh'
# Order by date, newest first
alias lt='la --sort=time --reverse'

alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias :q='exit'

alias d="kitty +kitten diff"

# Globbing should be done by Git itself
alias git='noglob git'
alias g='git'
alias gi='git'
alias gs='git status -s'
alias gpl='git pull origin $(git rev-parse --abbrev-ref HEAD)'
alias gps='git push origin $(git rev-parse --abbrev-ref HEAD)'
alias gd='git difftool --no-symlinks --dir-diff'
alias gr='git-refresh'
alias gh='git-history'

alias sudo='sudo -E '
alias mkdir='mkdir -p'
alias o='xdg-open'
alias fsize='df -PHT -x tmpfs -x devtmpfs -x squashfs'

alias diff='diff --color=auto'
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias egrep='egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias fgrep='fgrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'

alias sstat='systemctl status --no-pager --full'
alias sstart='sudo systemctl start'
alias sstop='sudo systemctl stop'

alias x='${MYFEXP} .'
