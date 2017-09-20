#!/bin/sh
remote=git://git.archlinux.org/svntogit/community.git
branch=packages/quassel
pkgname=quassel-client-custom
gitdir=$pkgname.git

rm $pkgname
ln -s $gitdir/trunk $pkgname

. ../pkg-patcher.sh
