#!/usr/bin/env bash

function doWork() {
    # https://github.com/alexmurray/emacs-snap/blob/master/snapcraft.yaml
    local arr=(
	# Build
	autoconf
	automake
	dbus-x11
	gcc-10
	libacl1-dev
	libasound2-dev
	libdbus-1-dev
	libgccjit-10-dev
	libgif-dev
	libgnutls28-dev
	libgpm-dev
	libgtk-3-dev
	libjansson-dev
	libjpeg-dev
	liblcms2-dev
	liblockfile-dev
	libm17n-dev
	libncurses5-dev
	liboss4-salsa2
	libotf-dev
	libpng-dev
	librsvg2-dev
	libselinux1-dev
	libsystemd-dev
	libtiff-dev
	libxml2-dev
	libxpm-dev
	libxt-dev
	procps
	quilt
	sharutils
	texinfo
	zlib1g-dev
	# Add build
	libgmp-dev
	libsqlite3-dev
	libwebkit2gtk-4.0-dev
	libmagickwand-dev
	libwebp-dev
	# Run
	gvfs
	libasound2
	libaspell15
	libasyncns0
	libatk-bridge2.0-0
	libatk1.0-0
	libatspi2.0-0
	libbrotli1
	libc6 # required for native-comp
	libc6-dev # required for native-comp
	libcairo-gobject2
	libcairo2
	libcanberra-gtk3-0
	libcanberra-gtk3-module
	libcanberra0
	libcroco3
	libdatrie1
	libdb5.3
	libdrm2
	libegl1
	libenchant1c2a
	libepoxy0
	libflac8
	libfontconfig1
	libfreetype6
	libgbm1
	libgccjit0
	libgcc-s1
	libgdk-pixbuf2.0-0
	libgif7
	libgl1
	libglvnd0
	libglx0
	libgpm2
	libgraphite2-3
	libgstreamer-gl1.0-0
	libgstreamer-plugins-base1.0-0
	libgstreamer1.0-0
	libgtk-3-0
	libgudev-1.0-0
	libharfbuzz-icu0
	libharfbuzz0b
	libhyphen0
	libice6
	libicu66
	libisl22
	libjansson4
	libjbig0
	libjpeg-turbo8
	liblcms2-2
	liblockfile1
	libltdl7
	libm17n-0
	libmpc3
	libmpfr6
	libnotify4
	libnss-mdns
	libnss-myhostname
	libnss-systemd
	libogg0
	liborc-0.4-0
	libotf0
	libpango-1.0-0
	libpangocairo-1.0-0
	libpangoft2-1.0-0
	libpixman-1-0
	libpng16-16
	libpulse0
	librsvg2-2
	libsasl2-2
	libsecret-1-0
	libsm6
	libsndfile1
	libsoup2.4-1
	libssl1.1
	libstdc++6
	libtdb1
	libthai0
	libtiff5
	libvorbis0a
	libvorbisenc2
	libvorbisfile3
	libwayland-client0
	libwayland-cursor0
	libwayland-egl1
	libwayland-server0
	libwebp6
	libwebpdemux2
	libwoff1
	libx11-6
	libx11-xcb1
	libxau6
	libxcb-render0
	libxcb-shm0
	libxcb1
	libxcomposite1
	libxcursor1
	libxdamage1
	libxdmcp6
	libxext6
	libxfixes3
	libxi6
	libxinerama1
	libxkbcommon0
	libxml2
	libxpm4
	libxrandr2
	libxrender1
	libxslt1.1
	libyajl2
	# Add runtime
	libgmp10
	mailutils
	libsqlite3-0
	imagemagick
	libwebkit2gtk-4.0-37
    )
    local pnames=""
    for i in "${arr[@]}"; do
	/usr/bin/dpkg -s "${i}" > /dev/null 2>&1 || pnames="${pnames} $i"
    done
    if [ -n "${pnames}" ]; then
	printf "Installing the following packages:\n%s\n" "${pnames}"
	sudo /usr/bin/apt-get update && sudo /usr/bin/apt-get install -y ${pnames}
    fi

    export CC=/usr/bin/gcc-10
    export CXX=/usr/bin/gcc-10

    if [[ -d ~/aur/tree-sitter ]]; then
	cd ~/aur/tree-sitter
	git fetch
    else
	mkdir -p ~/aur
	cd ~/aur/
	git clone https://github.com/tree-sitter/tree-sitter.git tree-sitter
    fi
    cd ~/aur/tree-sitter
    make
    echo "Installing tree-sitter library"
    sudo make install
    if command -v tree-sitter >/dev/null; then
	! ncu -gs -e 2 tree-sitter-cli && echo "Updating tree-sitter executable" && npm i -g tree-sitter-cli
    else
	echo "Installing tree-sitter executable"
	npm i -g tree-sitter-cli
    fi
    
    if [[ -d ~/aur/emacs-git/src ]]; then
	cd ~/aur/emacs-git/src
	git fetch
    else
	mkdir -p ~/aur/emacs-git/src
	cd ~/aur/emacs-git/src
	git clone https://git.savannah.gnu.org/git/emacs.git emacs-git
    fi
    cd ~/aur/emacs-git/src/emacs-git

    [[ -x configure ]] || ( ./autogen.sh )
    local _confflags="--sysconfdir=/etc \
    --prefix=/usr \
    --libexecdir=/usr/lib \
    --localstatedir=/var \
    --with-cairo \
    --with-harfbuzz \
    --with-libsystemd \
    --with-modules \
    --with-x-toolkit=gtk3 \
    --with-mailutils \
    --with-sound=alsa \
    --with-imagemagick \
    --with-json \
    --with-native-compilation=aot \
    --without-xaw3d \
    --without-gsettings \
    --without-gconf \
    --with-small-ja-dic \
    --with-xwidgets \
    --without-compress-install \
    --with-tree-sitter"

    export ac_cv_lib_gif_EGifPutExtensionLast=yes
    export CFLAGS="-march=native -O2 -pipe -fno-plt -fexceptions -Wp,-D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -fstack-clash-protection -fcf-protection"
    export CXXFLAGS="$CFLAGS -Wp,-D_GLIBCXX_ASSERTIONS"
    export LDFLAGS="-Wl,-O1,--sort-common,--as-needed,-z,relro,-z,now"
    export LTOFLAGS="-flto=auto"

    mkdir -p ~/aur/emacs-git/build
    cd ~/aur/emacs-git/build
    ~/aur/emacs-git/src/emacs-git/configure $_confflags
    make -j$(nproc) bootstrap
    sudo make install
}

doWork "$@"
