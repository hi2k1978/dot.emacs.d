#!/bin/sh
PREFIX="/usr/local"
echo "PREFIX = ${PREFIX}"

OPTIONS=""
OPTIONS="${OPTIONS} --with-xwidgets"
OPTIONS="${OPTIONS} --with-native-compilation"
OPTIONS="${OPTIONS} --without-xim"
echo "OPTIONS = ${OPTIONS}"

configure="./configure --prefix=${PREFIX} ${OPTIONS}"
echo $configure
eval $configure
