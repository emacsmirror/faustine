#!/bin/bash
# Install Emacs on Codeship - https://www.gnu.org/software/emacs
#
EMACS_DIR=${EMACS_DIR:=$HOME/cache/emacs}

set -e

if [ ! -d "${EMACS_DIR}" ]; then
  mkdir -p "${HOME}/emacs"
  wget http://ftp.gnu.org/gnu/emacs/emacs-25.2.tar.xz
  tar -xaf "emacs-25.2.tar.xz" --strip-components=1 --directory "${HOME}/emacs"

  (
    cd "${HOME}/emacs" || exit 1
    ./configure --prefix="${EMACS_DIR}" --with-xpm=no  2> /dev/null
    make
    make install
  )
fi

ln -s "${EMACS_DIR}/bin/"* "${HOME}/bin"


# Then as build steps call these two commands:

# curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

# export PATH="/home/rof/.cask/bin:$PATH"

