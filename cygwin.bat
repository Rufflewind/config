rem Start Command Prompt as Administrator, then run the following command:
rem
rem     bitsadmin /transfer cygwin ^
rem         https://github.com/Rufflewind/config/raw/master/cygwin.bat ^
rem         %TEMP%\cygwin.bat && %TEMP%\cygwin

set arch=x86
set cygroot=C:\cygwin
set pkgs=automake,autoconf,curl,diffutils,git,gcc,make,mintty,python,openssh

set mirror=http://mirror.cs.vt.edu/pub/cygwin/cygwin
set pkgcache=%cygroot%\var\cache\cygwin-setup
set cygsetup=%cygroot%\bin\cygsetup.exe

for %%x in (%cygsetup%) do mkdir %%~dpx
bitsadmin /transfer cygwin https://cygwin.com/setup-%arch%.exe %cygsetup%
%cygsetup% -dNq -l %pkgcache% -s %mirror% -P %pkgs% -R %cygroot%

rem Unblock SSH in Windows Firewall
netsh advfirewall firewall add rule action=allow dir=in ^
      name=ssh localport=22 protocol=tcp

rem Initialize shell configuration files, home directory, and SSH
rem (Q: would /bin/sh work here?)
%cygroot%\bin\mintty /bin/bash -l -c "ssh-host-config -y -w `base64 </dev/urandom | dd count=40 ibs=1 2>/dev/null`"

rem Start SSH daemon
%cygroot%\bin\cygrunsrv -S sshd
