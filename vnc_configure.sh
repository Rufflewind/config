#!/bin/sh
echo >&2 This script is mean to be read!
exit 1
set -eu

# --------------------------------------
# on server machine

# install
sudo pacman -S --needed tigervnc

# configure for user
sudo loginctl enable-linger "$USER"
mkdir -m 0700 "$HOME"/.vnc
# vncpasswd supports at most 8 characters
uuencode -m </dev/urandom /dev/stdout | tail -n +2 | dd bs=1 count=8 |
    vncpasswd -f >"$HOME"/.vnc/passwd
chmod 0600 "$HOME"/.vnc/passwd
cat >"$HOME"/.vnc/config <<EOF
localhost
EOF
cat >"$HOME"/.vnc/xstartup <<EOF
#!/bin/sh
exec startxfce4
EOF
chmod +x "$HOME"/.vnc/xstartup

# start the service
systemctl --user start vncserver@:1

# --------------------------------------
# on client machine

# copy password
mkdir -m 0700 "$HOME"/.vnc
scp -a ${REMOTE}:.vnc/passwd "$HOME"/.vnc/passwd-${REMOTE}

# port forward
ssh -fNL 15901:localhost:5901 ${REMOTE}
vncviewer localhost:15901
