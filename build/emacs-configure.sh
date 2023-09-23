#!/bin/sh
PREFIX="/usr/local"
echo "PREFIX = ${PREFIX}"

OPTIONS=""
OPTIONS="${OPTIONS} --with-native-compilation"
OPTIONS="${OPTIONS} --with-x-toolkit=gtk3"
OPTIONS="${OPTIONS} --without-xim"
OPTIONS="${OPTIONS} --with-imagemagick"
OPTIONS="${OPTIONS} --with-xft"
OPTIONS="${OPTIONS} --with-xwidgets"
echo "OPTIONS = ${OPTIONS}"

# sudo apt build-dep -y emacs
sudo apt install -y \
     gnutls-bin libgif-dev libxpm-dev \
     gnutls-dev libtinfo-dev libncurses-dev libacl1-dev
sudo apt install -y \
     libgccjit0 libgccjit-10-dev gcc-10
     
sudo apt install -y libxft-dev libxft2 # enable xft
sudo apt install -y imagemagick libmagick++-dev # enable imagemagick
sudo apt install -y librsvg2-dev # enable rsvg2
sudo apt install -y libwebp-dev webp # enable webp
sudo apt install -y libjansson4 libjansson-dev # enable json
sudo apt install -y libtree-sitter-dev # enable tree-sitter
sudo apt install -y libgtk-3-dev # enable gtk-3
sudo apt install -y libwebkit2gtk-4.0-dev # enable webkit
sudo apt install -y libgccjit0 libgccjit-11-dev # enable native-compile

configure="./configure --prefix=${PREFIX} ${OPTIONS}"
echo $configure
eval $configure
