#!/bin/sh

cp /dev/stdin /tmp/kitty_scrollback

emacsclient -cne "(ig-kitty-scrollback $1 $2 $3)"

kitty @ close-tab --match "title:#scrollback"
