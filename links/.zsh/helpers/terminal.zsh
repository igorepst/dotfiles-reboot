if [[ "${MYTERM}" = "xterm" ]]; then
        # Dirty hack. xterm doesn't support 24bit colors properly,
        # tmux does, but the COLORTERM env. variable isn't set
        # bat/delta uses this to show nicer colors
        # See https://gist.github.com/XVilka/8346728
        export COLORTERM=truecolor
fi
