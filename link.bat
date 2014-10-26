@echo off
cd %HOMEDRIVE%%HOMEPATH%
set RELDIR=stuff\config
rmdir     sbin
mklink /d sbin                %RELDIR%\home\sbin
del       .bashrc
mklink    .bashrc             %RELDIR%\home\.bashrc
del       .bash_profile
mklink    .bash_profile       %RELDIR%\home\.bash_profile
del       .profile
mklink    .profile             %RELDIR%\home\.profile
del       .emacs.d\init.el
mklink    .emacs.d\init.el    ..\%RELDIR%\home\.emacs.d\init.el
del                   .emacs.d\elisp\adwaita-custom-theme.el
mklink                .emacs.d\elisp\adwaita-custom-theme.el ^
  ..\..\%RELDIR%\home\.emacs.d\elisp\adwaita-custom-theme.el
del                   .emacs.d\elisp\gnuplot.el
mklink                .emacs.d\elisp\gnuplot.el ^
  ..\..\%RELDIR%\home\.emacs.d\elisp\gnuplot.el
del                   .emacs.d\elisp\solarized-custom-theme.el
mklink                .emacs.d\elisp\solarized-custom-theme.el ^
  ..\..\%RELDIR%\home\.emacs.d\elisp\solarized-custom-theme.el
del                   .emacs.d\elisp\tango-dark-custom-theme.el
mklink                .emacs.d\elisp\tango-dark-custom-theme.el ^
  ..\..\%RELDIR%\home\.emacs.d\elisp\tango-dark-custom-theme.el
del                   .emacs.d\elisp\wombat-custom-theme.el
mklink                .emacs.d\elisp\wombat-custom-theme.el ^
  ..\..\%RELDIR%\home\.emacs.d\elisp\wombat-custom-theme.el
