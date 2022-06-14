# http://zsh.sourceforge.net/Doc/Release/Command-Execution.html
# https://wiki.archlinux.org/title/Zsh#pacman_-F_%22command_not_found%22_handler
function command_not_found_handler() {
    local cmd="$1"
    if [ -x /usr/bin/pacman ]; then
	local purple='\e[1;35m' bright='\e[0;1m' green='\e[1;32m' reset='\e[0m'
	printf 'zsh: command not found: %s\n' "$1"
	local entries=(
            ${(f)"$(/usr/bin/pacman -F --machinereadable -- "/usr/bin/$1")"}
	)
	if (( ${#entries[@]} ))
	then
            printf "${bright}$1${reset} may be found in the following packages:\n"
            local pkg
            for entry in "${entries[@]}"
            do
		# (repo package version file)
		local fields=(
                    ${(0)entry}
		)
		if [[ "$pkg" != "${fields[2]}" ]]
		then
                    printf "${purple}%s/${bright}%s ${green}%s${reset}\n" "${fields[1]}" "${fields[2]}" "${fields[3]}"
		fi
		printf '    /%s\n' "${fields[4]}"
		pkg="${fields[2]}"
            done
	fi
	return 127
    elif [ -x /usr/lib/command-not-found ]; then
        /usr/lib/command-not-found -- "$cmd"
        return 127
    elif [ -x /usr/share/command-not-found/command-not-found ]; then
        /usr/share/command-not-found/command-not-found -- "$cmd"
        return 127
    fi
    printf "zsh: command not found: ${cmd}\n"
    return 127
}
