#!/bin/sh
exec sudo reflector --verbose --score 10 --sort rate --save /etc/pacman.d/mirrorlist
