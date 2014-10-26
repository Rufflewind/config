# ~/.profile for Bourne-compatible shells
#
# Note: If this script is run with the first argument set to `rc`, then it
#       will assume that this is not a login shell.  If invoked without `rc`,
#       this script may never return! (if it starts the X server)

# OSTYPE does not exist in every shell; we emulate it using `uname` but it
# does not always produce the exact same value
if [ -z "$OSTYPE" ]
then
    OSTYPE=`uname -s | tr "[:upper:]" "[:lower:]"`
    unset_ostype=t
fi

# `type` may give false positives, so prefer `command` if possible
# (but we're screwed if both `command` is missing and `type` is broken)
if type >/dev/null 2>&1 command
then
    command_exists() { command >/dev/null 2>&1 -v "$1"; }
else
    command_exists() { type >/dev/null 2>&1 "$1"; }
fi

# source machine-specific settings
[ -f "$HOME/.profile_local" ] && . "$HOME/.profile_local" "$1"

# login shells only
if [ "$1" != rc ]
then

    # command might not exist, in which case we just leave things as is
    { emacs_path=`command 2>/dev/null -v emacs`; } 2>/dev/null
    if [ "$emacs_path" ]
    then
        EDITOR="$emacs_path -nw"
        export EDITOR
    fi
    unset emacs_path

    # beeps are annoying
    LESS=$LESS-qR
    export LESS

    # enable colored GCC diagnostics
    GCC_COLORS="caret=01;32:locus=01:quote=01"${GCC_COLORS:+:}$GCC_COLORS
    GCC_COLORS="error=01;31:warning=01;35:note=01;36":$GCC_COLORS
    export GCC_COLORS

    # avoid adding extra colons
    # (note that PATH shouldn't ever be unset so we needn't worry about that)
    C_INCLUDE_PATH=${C_INCLUDE_PATH+:}$C_INCLUDE_PATH
    C_INCLUDE_PATH=/usr/local/include$C_INCLUDE_PATH
    CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH+:}$CPLUS_INCLUDE_PATH
    CPLUS_INCLUDE_PATH=/usr/local/include$CPLUS_INCLUDE_PATH
    LD_LIBRARY_PATH=${LD_LIBRARY_PATH+:}$LD_LIBRARY_PATH
    LD_LIBRARY_PATH=/usr/local/lib$LD_LIBRARY_PATH
    LIBRARY_PATH=${LIBRARY_PATH+:}$LIBRARY_PATH
    LIBRARY_PATH=/usr/local/lib$LIBRARY_PATH

    case $OSTYPE in
        *msys*)

            # do not use the `PATH` variable from Windows because it has a lot
            # of garbage that can severely reduce the performance of the shell
            PATH=/usr/local/bin:/usr/bin:/bin

            # to use MSYS with a different MinGW, set the `MINGW_PATH` to its
            # corresponding `bin` directory (do we also need to worry about
            # the libs as well?)
            if [ -n "$MINGW_PATH" ]
            then
                PATH=$MINGW_PATH:$PATH
            fi
            unset MINGW_PATH

            # Python
            PATH=$PATH:/c/Python27

            # Git
            # - must contain `libcore` in its parent directory otherwise
            #   commands like `git pull` won't work.
            # - must NOT override Mingw commands or terminal will freeze up
            PATH=$PATH:/c/Git/bin

            # we need `ping` because one of Rust's tests requires it, but it
            # MUST occur after other paths due to conflicts with, say, `find`.
            PATH=$PATH:/c/Windows/system32

            ;;
        *cygwin*)

            # The CYGWIN variable is special: it must be set in
            # System Properties -> System variables, not here!
            #
            # CYGWIN=nodosfilewarning

            program_files="/cygdrive/c/Program Files"
            program_files_86="$program_files (x86)"

            # don't use Windows' PATH variable because it's full of garbage that
            # makes everything incredibly slow (as if Windows isn't slow enough)
            PATH=/usr/local/bin:/usr/bin
            PATH=$PATH:/cygdrive/c/Windows/system32

            # Doxygen
            PATH=$program_files/doxygen/bin:$PATH

            # Haskell
            # (we use the CYG_GHC_ROOT variable defined in Windows,
            #  which must be in Cygwin's path format)
            if [ "$CYG_GHC_ROOT" ]
            then
                PATH=$CYG_GHC_ROOT/bin:$CYG_GHC_ROOT/lib/extralibs/bin:$PATH
                PATH=$HOME/AppData/Roaming/cabal/bin:$PATH
            fi

            # Nodejs
            # PATH=$program_files_86/nodejs:$PATH
            # PATH=$HOME/AppData/Roaming/npm:$PATH

            # Rust
            PATH=$program_files_86/Rust/bin:$PATH

            unset program_files program_files_86

            DISPLAY=${DISPLAY:=:0}
            export DISPLAY

            ;;
        *)

            PATH=$HOME/.cabal/bin:$PATH
            PATH=$HOME/.gem/ruby/2.1.0/bin:$PATH
            C_INCLUDE_PATH=$HOME/include:$C_INCLUDE_PATH
            CPLUS_INCLUDE_PATH=$HOME/include:$CPLUS_INCLUDE_PATH
            LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH
            LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH

            # number of threads in OpenMP
            OMP_NUM_THREADS=`nproc`
            export OMP_NUM_THREADS

            # if USE_GPG_AGENT is set, enable GnuPG authentication agent for SSH
            if [ "$USE_GPG_AGENT" ]
            then
                gpg_env_file=$HOME/.gnupg-envs
                pgrep >/dev/null 2>&1 -xu "$USER" gpg-agent || {
                    gpg-agent >"$gpg_env_file" -s --daemon --enable-ssh-support
                    chmod +x "$gpg_env_file"
                }
                . "$gpg_env_file"
            fi

            ;;
    esac

    # add ~/sbin and ~/bin/ to the PATH variable
    PATH=$HOME/sbin:$HOME/bin:$PATH
    export C_INCLUDE_PATH CPLUS_INCLUDE_PATH LD_LIBRARY_PATH LIBRARY_PATH PATH

fi

case $OSTYPE in
    *cygwin*|*msys*);;
    *)

        # GnuPG agent uses `pinentry`, which needs to know the current TTY
        GPG_TTY=`tty`
        export GPG_TTY

        # environment modules
        if command_exists modulecmd
        then
            type >/dev/null 2>&1 module || {
                # quoting of eval is needed in most shells (except zsh)
                module() { eval "`modulecmd sh "$@"`"; }
                module use /etc/environment-modules.d
                MODULERCFILE=/etc/environment-modules
                export MODULERCFILE
            }
        fi

        # some systems don't know urxvt, so let's pretend to be xterm;
        # however, don't fake as `xterm-256color` because emacs colors would
        # look weird
        if [ "$RXVT_COMPAT" ]; then
            case $TERM in
                rxvt-unicode*)
                    TERM=xterm-16color
                    export TERM
                    ;;
            esac
        fi

esac

# aliases
if command_exists alias
then
    alias gita="git add"
    alias gitau="git add -u"
    alias gitb="git rebase"
    alias gitbi="git rebase -i"
    alias gitbc="git rebase --continue"
    alias gitc="git commit"
    alias gitca="git commit --amend"
    alias gitcan="git commit --amend --no-edit"
    alias gitd="git diff"
    alias gitdn="git diff --no-index"
    alias gitdc="git diff --cached"
    alias gith="git checkout"
    alias gitl="git log"
    alias gitu="git pull"
    alias gitp="git push"
    alias gitr="git reset"
    alias gits="git status"
    alias grep="grep --color=auto"
    alias R="R --no-save"
    # hide Windows system files
    alias ls="ls --color=auto '-Intuser.*' '-INTUSER.*'"
fi

# bash/bsh-specific
if [ "$BASH_VERSION$ZSH_VERSION" ]
then
    HISTCONTROL=erasedups:ignoreboth
    HISTSIZE=1000
    SAVEHIST=1000
fi

# cleanup
if [ "$unset_ostype" ]
then
    unset unset_ostype OSTYPE
fi
unset command_exists

# start X server if needed
if [ "$1" != rc ]
then
    case $OSTYPE in
        *cygwin*|*msys*);;
        *)
            if [ -z "$DISPLAY" ] && [ "`fgconsole 2>/dev/null`" = 1 ]
            then
                command 2>/dev/null -v startx && exec startx
            fi
            ;;
    esac
fi
