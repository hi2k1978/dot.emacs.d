#!/bin/sh
PREFIX="/usr/local"
echo "PREFIX = ${PREFIX}"

OPTIONS=""
OPTIONS="${OPTIONS} --with-xwidgets"
OPTIONS="${OPTIONS} --with-native-compilation"
OPTIONS="${OPTIONS} --with-x-toolkit=gtk3"
OPTIONS="${OPTIONS} --without-xim"
echo "OPTIONS = ${OPTIONS}"

sudo apt build-dep -y emacs
sudo apt install -y \
     libjansson4 libjansson-dev gnutls-bin libgif-dev libxpm-dev \
     gnutls-dev libtinfo-dev libncurses-dev
sudo apt install -y libgtk-3-dev # gtk-3
sudo apt install -y libwebkit2gtk-4.0-dev # webkit
sudo apt install -y libgccjit0 libgccjit-11-dev # native-compile
configure="./configure --prefix=${PREFIX} ${OPTIONS}"
echo $configure
eval $configure
