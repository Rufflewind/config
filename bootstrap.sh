#!/bin/sh
#
# curl -L https://github.com/Rufflewind/config/bootstrap.sh | sh
#
{
    sudo pacman -S --needed git
    git clone https://github.com/Rufflewind/config "$HOME/stuff/config"
    cd "$HOME/stuff/config"
    ./install.sh
}
