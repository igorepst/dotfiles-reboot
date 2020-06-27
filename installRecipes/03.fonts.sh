#!/usr/bin/env bash

function doWork(){
	local tmpdir=$(mktemp -d) 
	echo 'Installing fonts:'
	echo 'Installing DejaVu Sans Mono Nerd Font Complete Mono.ttf'
	curl -o "$tmpdir/DejaVu Sans Mono Nerd Font Complete Mono.ttf" -L -O https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Regular/complete/DejaVu%20Sans%20Mono%20Nerd%20Font%20Complete%20Mono.ttf
	echo 'Installing DejaVu Sans Mono Bold Nerd Font Complete Mono.ttf'
	curl -o "$tmpdir/DejaVu Sans Mono Bold Nerd Font Complete Mono.ttf" -L -O https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Bold/complete/DejaVu%20Sans%20Mono%20Bold%20Nerd%20Font%20Complete%20Mono.ttf

	echo 'Converting TTF to OTF for pango'
	cat >"$tmpdir"/convertFonts.py <<"EOF"
import fontforge
import os
fonts = [f for f in os.listdir('.') if f.endswith('.ttf')]
for font in fonts:
	f = fontforge.open(font)
	f.generate(font[:-3] + 'otf')
EOF
	pushd "$tmpdir" >/dev/null
	python ./convertFonts.py
	popd >/dev/null
	local fdir=~/.local/share/fonts
	mkdir -p "$fdir"
	mv -f -t "$fdir" "$tmpdir/"*.[to]tf
	rm -rf "$tmpdir"
	fc-cache

	if [ ! -f /etc/vconsole.conf ]; then
		echo 'Setting /etc/vconsole.conf'
		sudo sh -c 'cat >/etc/vconsole.conf <<"EOF"
FONT=ter-122n
FONT_MAP=8859-1
EOF'
	fi
}

doWork
