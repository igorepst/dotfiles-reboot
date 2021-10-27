#!/bin/sh

[ -f ~/.xprofile ] && source ~/.xprofile

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

case "${MY_PC_IS}" in
    home)
        exec startplasma-x11
        ;;
    *)
        ;;
esac

