#!/bin/sh

s_in=/tmp/kitty_scrollback
cp /dev/stdin "${s_in}"

sed -i 's/\]8;;file:[^\\]*\\//g; s/\]133;[AC]\\\//g; s/\]8;;\\\//g' "${s_in}"

emacsclient -cne "(ig-kitty-scrollback \"${s_in}\")"

kitty @ close-tab --match "title:#scrollback"
