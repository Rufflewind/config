@echo off
cd %HOMEDRIVE%%HOMEPATH%
set RELDIR=stuff\config
rmdir     bin
mklink /d bin                 %RELDIR%\home\bin
del       .bashrc
mklink    .bashrc             %RELDIR%\home\.bashrc
del       .bash_profile
mklink    .bash_profile       %RELDIR%\home\.bash_profile
del       .emacs.d\init.el
mklink    .emacs.d\init.el    ..\%RELDIR%\home\.emacs.d\init.el
rmdir     .emacs.d\elisp
mklink /d .emacs.d\elisp      ..\%RELDIR%\home\.emacs.d\elisp
