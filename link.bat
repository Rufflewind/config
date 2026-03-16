@echo off
cd %HOMEDRIVE%%HOMEPATH%
set base=stuff\config
mkdir     .local\sbin
rmdir     .local\sbin
mklink /d .local\sbin                     ..\%base%\home\sbin
del       .bashrc
mklink    .bashrc                            %base%\home\.bashrc
del       .bash_profile
mklink    .bash_profile                      %base%\home\.bash_profile
del       .profile
mklink    .profile                           %base%\home\.profile
mkdir     .emacs.d\elisp
del       .emacs.d\config.el
mklink    .emacs.d\config.el              ..\%base%\home\.emacs.d\config.el
