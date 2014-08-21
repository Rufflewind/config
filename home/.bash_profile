. ~/.bashrc

# set editor to emacs if available
EMACS_PATH=$(command -v emacs 2>/dev/null) && \
    export EDITOR="$EMACS_PATH -nw"
unset EMACS_PATH

# beeps are annoying
export LESS="-qR"

export C_INCLUDE_PATH="/usr/local/include:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="/usr/local/include:$CPLUS_INCLUDE_PATH"
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"

# defined in bashrc
case "$SYSTEM" in
    MINGW*)

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
    CYGWIN*)
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

        unset PROGRAM_FILES PROGRAM_FILES_86 HASKELL_PATH
        ;;
    *)
        export PATH="$HOME/.cabal/bin:$PATH"
        export PATH="$HOME/.gem/ruby/2.1.0/bin:$PATH"
        export C_INCLUDE_PATH="$HOME/include:$C_INCLUDE_PATH"
        export CPLUS_INCLUDE_PATH="$HOME/include:$CPLUS_INCLUDE_PATH"
        export LIBRARY_PATH="$HOME/lib:$LIBRARY_PATH"
        export LD_LIBRARY_PATH="$HOME/lib:$LD_LIBRARY_PATH"

        # start/enable authentication agent
        if [ "$USE_GPG_AGENT" ]; then
            GPG_ENV_FILE="$HOME/.gnupg-envs"
            if ! pgrep >/dev/null 2>&1 -xu "$USER" gpg-agent; then
                gpg-agent >"$GPG_ENV_FILE" -s --daemon --enable-ssh-support
                chmod +x "$GPG_ENV_FILE"
            fi
            . "$GPG_ENV_FILE"
        fi

        ;;
esac

# add ~/bin to the PATH variable
export PATH="$HOME/bin:$PATH"

# defined in bashrc
case "$SYSTEM" in
    Msys);;
    Cygwin);;
    *)
        # start X
        [[ -z "$DISPLAY" && "`fgconsole 2>/dev/null`" = 1 ]] && exec startx;;
esac
