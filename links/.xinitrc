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
	hsetroot -center ~/.theme/background.png &
	! ping -qc 1 -w 1 8.8.8.8 > /dev/null 2>&1 && sudo systemctl restart iwd.service
	eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
	export SSH_AUTH_SOCK
	export XDG_CURRENT_DESKTOP=GNOME
	udiskie --tray &
	picom -b
        exec awesome >>/tmp/awesome-stdout 2>>/tmp/awesome-stderr
        ;;
    *)
        ;;
esac

