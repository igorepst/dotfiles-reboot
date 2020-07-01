#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

currentThemeXColors=$HOME/.colors/currentTheme.Xcolors
if [ -f "$currentThemeXColors" ]; then
    xrdb -merge "$currentThemeXColors"
fi

currentThemeXResources=$HOME/.colors/currentTheme.Xresources
if [ -f "$currentThemeXResources" ]; then
    xrdb -merge "$currentThemeXResources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

if [ -f /usr/bin/VBoxClient-all ]; then 
    VBoxClient-all
    xrandr --output Virtual1 --mode 1440x900
fi

exec awesome
