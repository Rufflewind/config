. ~/.bashrc

# set editor to emacs if available
EMACS_PATH=$(command -v emacs 2>/dev/null) && \
    export EDITOR="\"$EMACS_PATH\" -nw"
unset EMACS_PATH

# defined in bashrc
case "$SYSTEM" in
    Msys)

        # don't use Windows' `PATH` variable because it's full of garbage that
        # makes everything incredibly slow (as if Windows isn't slow enough)
        export PATH="$MINGW_PATH:/usr/local/bin:/usr/bin:/bin"

        # MinGW (development tools): to use Msys with a different set of dev
        # tools, set the `MINGW_PATH` to its corresponding `bin` directory
        [ -n "$MINGW_PATH" ] || export PATH="$MINGW_PATH:$PATH"
        unset MINGW_PATH

        # Python
        export PATH="$PATH:/c/Python27"

        # Git
        # - must contain `libcore` in its parent directory otherwise commands
        #   like `git pull` won't work.
        # - must NOT override MinGW commands otherwise terminal will freeze up
        export PATH="$PATH:/c/Git/bin"

        # we need `ping` because one of Rust's tests requires it, but it MUST
        # occur after other paths due to conflicts with, say, `find`.
        export PATH="$PATH:/c/Windows/system32"

        ;;
    Cygwin)
        PROGRAM_FILES="/cygdrive/c/Program Files"
        PROGRAM_FILES_86="$PROGRAM_FILES (x86)"
        HASKELL_PATH="$PROGRAM_FILES_86/Haskell Platform/2013.2.0.0"

        export CYGWIN=nodosfilewarning
        export DISPLAY=:0.0

        # don't use Windows' PATH variable because it's full of garbage that
        # makes everything incredibly slow (as if Windows isn't slow enough)
        export PATH="/usr/local/bin:/usr/bin"
        export PATH="$PATH:/cygdrive/c/Windows/system32"

        # Doxygen
        export PATH="$PROGRAM_FILES/doxygen/bin:$PATH"

        # Haskell
        export PATH="$HASKELL_PATH/bin:$HASKELL_PATH/lib/extralibs/bin:$PATH"
        export PATH="$HOME/AppData/Roaming/cabal/bin:$PATH"

        # Nodejs
        # export PATH="$PROGRAM_FILES_86/nodejs:$PATH"
        # export PATH="$HOME/AppData/Roaming/npm:$PATH"

        # Rust
        export PATH="$PROGRAM_FILES_86/Rust/bin:$PATH"

        # (Cgywin) GCC does not use /usr/local/... for some reason
        export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"

        unset PROGRAM_FILES HASKELL_PATH
        ;;
esac

# add ~/bin to the PATH variable
export PATH="$HOME/bin:$PATH"

# start X
case "$HOSTNAME" in
    *-g73jh) [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx;;
esac
