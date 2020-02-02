#!/bin/bash
set -eux

##ROOT: pacman -S --needed --noconfirm cuda nvidia
##ROOT: pacman -S --needed --noconfirm graphicsmagick snappy
# For Torch:
##ROOT: pacman -S --needed --noconfirm cmake curl readline ncurses gcc gcc-fortran unzip libjpeg-turbo libpng libpng imagemagick graphicsmagick fftw sox zeromq ipython qt5-webkit
##WHEEL: yay -S --needed openblas-lapack

# Build under restricted user
git clone --depth 1 https://github.com/nagadomi/waifu2x.git
git clone --recursive https://github.com/nagadomi/distro.git torch
(
    PATH=/opt/cuda/bin:$PATH
    export TORCH_NVCC_FLAGS=-D__CUDA_NO_HALF_OPERATORS__
    cd torch
    echo no | ./install.sh
)
waifu2x/install_lua_modules.sh
echo 'cd ~/waifu2x && . ../torch/install/bin/torch-activate && exec th waifu2x.lua -crop_size 128 "$@"' >waifu2x.sh
chmod +x waifu2x.sh

# Execution:
##WHEEL: sudo -u waifu2x ~waifu2x/waifu2x.sh -m noise_scale -noise_level 2 -i /tmp/input.png -o /tmp/output.png
