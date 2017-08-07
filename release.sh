#!/bin/sh

APP='njconnect'
VER=$(sed -ne 's/^#define VERSION "\(.*\)".*/\1/p' njconnect.c)
DST=${1:-.}
PKG="${DST}/${APP}-${VER}.tar.xz"

rm -fr /tmp/${APP}-${VER}

cp -r . /tmp/${APP}-${VER}

echo "${PKG}"

tar cJv -C /tmp --exclude-vcs -f ${PKG} ${APP}-${VER}/
