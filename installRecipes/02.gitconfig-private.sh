#!/usr/bin/env bash

function doWork() {
	echo 'Checking private Git configuration'
	local gcp=~/.gitconfig-private
	if [ ! -f "$gcp" ]; then
		read -p "Git username? " gituser
		read -p "Git email? " gitemail
		echo '[User]' >"$gcp"
		echo 'name =' $gituser>>"$gcp"
		echo 'email =' $gitemail>>"$gcp"
		chmod 600 "$gcp"
	fi
}

doWork
