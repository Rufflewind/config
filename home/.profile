# ~/.profile for Bourne-compatible shells
#
# Note: If this script is run with the first argument set to `rc`, then it
#       will assume that this is not a login shell.  If invoked without `rc`,
#       this script may never return! (if it starts the X server)

# OSTYPE does not exist in every shell; we emulate it using `uname` but it
# does not always produce the exact same value
if [ -z "${OSTYPE-}" ]
then
    OSTYPE=`uname -s | tr "[:upper:]" "[:lower:]"`
    unset_ostype=t
fi

# `type` may give false positives, so prefer `command` if possible
# (but we're screwed if both `command` is missing and `type` is broken)
if type >/dev/null 2>&1 command
then command_exists() { command >/dev/null 2>&1 -v "$1"; }
else command_exists() { type >/dev/null 2>&1 "$1"; }
fi

# source machine-specific settings
[ -f "$HOME/.profile_local" ] && . "$HOME/.profile_local" "$1"

# login shells only
if [ "${1-}" != rc ]
then

    # command might not exist, in which case we just leave things as is
    {
        emacs_path=`command -v emacs || :`
    } 2>/dev/null
    if [ "$emacs_path" ]
    then
        EDITOR="$emacs_path -nw"
        export EDITOR
    fi
    unset emacs_path

    # beeps are annoying
    LESS=${LESS-}-qRS
    export LESS
    {
        highlight_path=`command -v highlight || :`
    } 2>/dev/null
    if [ "$highlight_path" ]; then
        LESSOPEN="| $highlight_path %s -qO xterm256 -s solarized-light --force"
        export LESSOPEN
    fi
    unset highlight_path

    # enable colored GCC diagnostics
    GCC_COLORS="caret=01;32:locus=01:quote=01"${GCC_COLORS:+:}${GCC_COLORS-}
    GCC_COLORS="error=01;31:warning=01;35:note=01;36":$GCC_COLORS
    export GCC_COLORS

    # avoid adding extra colons
    # (note that PATH shouldn't ever be unset so we needn't worry about that)
    C_INCLUDE_PATH=${C_INCLUDE_PATH+:}${C_INCLUDE_PATH-}
    C_INCLUDE_PATH=/usr/local/include$C_INCLUDE_PATH
    CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH+:}${CPLUS_INCLUDE_PATH-}
    CPLUS_INCLUDE_PATH=/usr/local/include$CPLUS_INCLUDE_PATH
    LD_LIBRARY_PATH=${LD_LIBRARY_PATH+:}${LD_LIBRARY_PATH-}
    LD_LIBRARY_PATH=/usr/local/lib$LD_LIBRARY_PATH
    LIBRARY_PATH=${LIBRARY_PATH+:}${LIBRARY_PATH-}
    LIBRARY_PATH=/usr/local/lib$LIBRARY_PATH

    case $OSTYPE in
        *cygwin*|*msys*)

            # do not use the `PATH` variable from Windows because it has a lot
            # of garbage that can severely reduce the performance of the shell
            PATH=/usr/local/bin:/usr/bin:/bin

            # needed by Git Bash
            if [ -d /mingw64/bin ]
            then PATH=/mingw64/bin:$PATH
            fi

            # we need `ping` because one of Rust's tests requires it, but it
            # MUST occur after other paths due to conflicts with, say, `find`.
            PATH=$PATH:/c/Windows/system32

            # to use MSYS with a different MinGW, set the `MINGW_PATH` to its
            # corresponding `bin` directory (do we also need to worry about
            # the libs as well?)
            [ -z "${EXTRA_PATH-}" ] || PATH=$EXTRA_PATH:$PATH
            unset EXTRA_PATH

            ;;
    esac

    case $OSTYPE in
        *cygwin*)

            # The CYGWIN variable is special: it must be set in
            # System Properties -> System variables, not here!
            #
            # CYGWIN=nodosfilewarning

            # Lapack
            PATH=/usr/lib/lapack:$PATH

            # Doxygen
            PATH=/cygdrive/c/ProgramFiles/doxygen/bin:$PATH

            # Haskell
            # (we use the CYG_GHC_ROOT variable defined in Windows,
            #  which must be in Cygwin's path format)
            if [ "${CYG_GHC_ROOT-}" ]
            then
                PATH=$CYG_GHC_ROOT/bin:$CYG_GHC_ROOT/lib/extralibs/bin:$PATH
                PATH=$HOME/AppData/Roaming/cabal/bin:$PATH
            fi

            # Nodejs
            # PATH=/cygdrive/c/ProgramFilesX86/nodejs:$PATH
            # PATH=$HOME/AppData/Roaming/npm:$PATH

            # Rust
            PATH=/cygdrive/c/ProgramFilesX86/Rust/bin:$PATH

            : ${DISPLAY=:0}
            export DISPLAY

            ;;
        *msys*)

            # Python
            PATH=$PATH:/c/ProgramFilesX86/Python27

            # Git
            # - must contain `libcore` in its parent directory otherwise
            #   commands like `git pull` won't work.
            # - must NOT override Mingw commands or terminal will freeze up
            PATH=$PATH:/c/ProgramFiles/Git/bin

            ;;
        *)

            # find the latest Ruby version
            ruby_version_patt='s/.*\([0-9]\{1,\}\.[0-9]\{1,\}\)\..*/\1.0/'
            { ruby_version=`ruby --version | sed "$ruby_version_patt" || :`
            } 2>/dev/null
            if [ "$ruby_version" ]
            then PATH=$HOME/.gem/ruby/$ruby_version/bin:$PATH
            fi
            unset ruby_version ruby_version_patt

            PATH=$HOME/.cabal/bin:$HOME/.cargo/bin:$PATH
            C_INCLUDE_PATH=$HOME/.local/include:$C_INCLUDE_PATH
            CPLUS_INCLUDE_PATH=$HOME/.local/include:$CPLUS_INCLUDE_PATH
            LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH
            LD_LIBRARY_PATH=$HOME/.local/lib:$LD_LIBRARY_PATH
            MYPYPATH=${MYPYPATH+:}${MYPYPATH-}
            MYPYPATH=$HOME/stuff$MYPYPATH
            PKG_CONFIG_PATH=${PKG_CONFIG_PATH+:}${PKG_CONFIG_PATH-}
            PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig$PKG_CONFIG_PATH
            PKG_CONFIG_PATH=$HOME/.local/share/pkgconfig:$PKG_CONFIG_PATH

            # if USE_GPG_AGENT is set, enable GnuPG authentication agent for SSH
            if [ "${USE_GPG_AGENT-}" ]
            then
                gpg_env_file=$HOME/.gnupg-envs
                pgrep >/dev/null 2>&1 -xu "$USER" gpg-agent || {
                    gpg-agent >"$gpg_env_file" -s --daemon --enable-ssh-support
                    chmod +x "$gpg_env_file"
                }
                . "$gpg_env_file"
            fi

            TEXMFHOME=$HOME/.config/texmf
            export TEXMFHOME

            alias pacman-upgrade="sudo pacman -Squy --noprogressbar"
            alias pacaur-upgrade="pacaur -Squy --noprogressbar"

            ;;
    esac

    if [ -d "$HOME/stuff/devutils" ]
    then PATH=$HOME/stuff/devutils/bin:$PATH
    fi

    # add ~/sbin and ~/bin/ to the PATH variable
    PATH=$HOME/.local/sbin:$HOME/.local/bin:$PATH
    export C_INCLUDE_PATH CPLUS_INCLUDE_PATH LD_LIBRARY_PATH LIBRARY_PATH MYPYPATH PATH

    # customize the color scheme for Linux terminals
    if [ "${TERM-}" = linux ]
    then
        printf "\033]P0000000" # black/bg
        printf "\033]P1803232" # darkred
        printf "\033]P25b762f" # darkgreen
        printf "\033]P3aa9943" # brown
        printf "\033]P4324c80" # darkblue
        printf "\033]P5706c9a" # darkmagenta
        printf "\033]P692b19e" # darkcyan
        printf "\033]P7ffffff" # lightgray
        printf "\033]P8222222" # darkgray
        printf "\033]P9982b2b" # red
        printf "\033]PA89b83f" # green
        printf "\033]PBefef60" # yellow
        printf "\033]PC2b4f98" # blue
        printf "\033]PD826ab1" # magenta
        printf "\033]PEa1cdcd" # cyan
        printf "\033]PFdedede" # white/fg
    fi

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
        if [ "${RXVT_COMPAT-}" ]; then
            case ${TERM-} in
                rxvt-unicode*)
                    TERM=xterm-16color
                    export TERM
                    ;;
            esac
        fi

esac

LS_COLORS='di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01'
LS_COLORS=$LS_COLORS':cd=40;33;01:or=40;31;01:su=37;41:sg=30;43'
LS_COLORS=$LS_COLORS':tw=30;42:ow=30;44:st=37;44:ex=01;32:*.tar=01;31'
LS_COLORS=$LS_COLORS':*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31'
LS_COLORS=$LS_COLORS':*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31'
LS_COLORS=$LS_COLORS':*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31'
LS_COLORS=$LS_COLORS':*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31'
LS_COLORS=$LS_COLORS':*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31'
LS_COLORS=$LS_COLORS':*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31'
LS_COLORS=$LS_COLORS':*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35'
LS_COLORS=$LS_COLORS':*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35'
LS_COLORS=$LS_COLORS':*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35'
LS_COLORS=$LS_COLORS':*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35'
LS_COLORS=$LS_COLORS':*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35'
LS_COLORS=$LS_COLORS':*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35'
LS_COLORS=$LS_COLORS':*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35'
LS_COLORS=$LS_COLORS':*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35'
LS_COLORS=$LS_COLORS':*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35'
LS_COLORS=$LS_COLORS':*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35'
LS_COLORS=$LS_COLORS':*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35'
LS_COLORS=$LS_COLORS':*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35'
LS_COLORS=$LS_COLORS':*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36'
LS_COLORS=$LS_COLORS':*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36'
LS_COLORS=$LS_COLORS':*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36'
LS_COLORS=$LS_COLORS':*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36'
LS_COLORS=$LS_COLORS':*.spx=00;36:*.xspf=00;36:*.shar=01;31'
export LS_COLORS

# don't try to sort things "smartly"
LC_COLLATE=C
export LC_COLLATE

# aliases
if command_exists alias
then
    alias bower=bower_wrapper
    alias cabal=qcabal
    alias diff='diff --color'
    alias ghc='ghc -fforce-recomp'
    alias gpgp="printf 'Passphrase: ' &&
                { getpass; cat; } |
                gpg --passphrase-fd 0 --pinentry-mode=loopback"
    alias grep="grep --color=auto"
    alias hexdump="hexdump -C"
    alias R="R --no-save"
    alias pdflatexmk="latexmk -cd -g -pdf -interaction=nonstopmode"
    alias luks="sudo luks-${USER}"
    # hide Windows system files
    alias ls="ls --quoting-style=literal --color=auto \
              --group-directories-first '-Intuser.*' '-INTUSER.*'"
    alias man='LESS_TERMCAP_md="[1;33m" LESS_TERMCAP_me="[0m" LESS_TERMCAP_us="[4;36m" LESS_TERMCAP_ue="[0m" man'
    alias pacdiff='sudo DIFFPROG="diff -u --color" pacdiff'
    alias sneppy='snep sync "$HOME/stuff/_urandom/utils.py"'
    alias snepsh='snep sync "$HOME/stuff/_urandom/shell-utility.sh"'
    alias sshfs="sshfs -o ssh_command='ssh -S none'"
    alias userctl="systemctl --user"
    alias wanip='dig +short myip.opendns.com @resolver1.opendns.com'
fi

pkill() {
    echo Use killall -g instead
}

bower_wrapper() {
    \bower "$@" &&
    for arg
    do
        if [ "${arg}" = --save ] && [ -f bower.json ]; then
            bower-sort-deps bower.json
            break
        fi
    done
}

duhs() {
    du -hs "$@" | sort -h
}

grepr() {
    set -- "$@" "$1" &&
    shift &&
    grep "$@"
}

hist() {
    grep -a --color=auto "$@" "$HOME/.histfile"
}


# similar to `tree`, but uses `less` as a pager and supports `-n` as a synonym
# for `--filelimit`
treeless() (
    set -eu
    i=0
    count=$#
    parsing_filelimit=
    while [ "$i" -lt "$count" ]; do
        arg=$1
        shift
        if [ "$parsing_filelimit" ]; then
            set -- "$@" --filelimit="$arg"
            parsing_filelimit=
        else
            case $arg in
                -n)
                    parsing_filelimit=t;;
                -n*)
                    arg=`printf '%s' "$arg" | sed s/^-n//`
                    set -- "$@" --filelimit="$arg";;
                *)
                    set -- "$@" "$arg";;
            esac
        fi
        i=`expr "$i" + 1`
    done
    tree -C "$@" | less
)


pnd() {
    mkdir -p "$1" && pushd "$1"
}

uppercase() {
    tr "[[:lower:]]" "[[:upper:]]"
}

lowercase() {
    tr "[[:upper:]]" "[[:lower:]]"
}

gittd() {
    if [ $# -ne 0 ]; then
        git stash &&
        "$@" &&
        git stash pop
    else
        git stash &&
        echo >&2
        echo >&2 "Entering new shell.  Type 'exit' to unstash."
        "${SHELL}" &&
        git stash pop
    fi
}

# undo the effect of Alt + SysRq + r
reraw() {
    sudo kbd_mode -s -C /dev/tty1
}

texc() (
    if [ $# -ne 1 ]; then
        cat >&2 <<"EOF"
usage: texc <input>

This is just a convenient alias for:

    latexmk -verbose -g -pdf -interaction=nonstopmode -cd <input>
EOF
        return 1
    fi
    dir=`dirname "$1"` &&
    latexmk -verbose -g -pdf -interaction=nonstopmode -cd "$1"
)

identify_fps() {
    identify -format "Frame %s: %Tcs\n" "$@"
}

pkgbuild_env() {
    d=`mktemp -d` &&
    cp PKGBUILD "$d/PKGBUILD" &&
    (cd "$d" && "$SHELL")
}

# bash/zsh-specific
if [ "${BASH_VERSION-}${ZSH_VERSION-}" ]
then
    HISTCONTROL=erasedups:ignoreboth
    HISTSIZE=1000
    SAVEHIST=1000
fi

# cleanup
if [ "${unset_ostype-}" ]
then unset unset_ostype OSTYPE
fi
unset command_exists

# start X server if needed
if [ "${1-}" != rc ]
then
    case ${OSTYPE-} in
        *cygwin*|*msys*);;
        *)
            if [ -z "${DISPLAY-}" ] &&
               [ "`fgconsole 2>/dev/null`" = 1 ] &&
               [ "${TERM-}" = linux ] &&
               command >/dev/null 2>&1 -v startx &&
               command >/dev/null 2>&1 -v xset
            then
                sudo "/usr/local/sbin/luks-${USER}" open || :
                ps -A -o comm | grep >/dev/null 2>&1 '^X\(org\)\{0,1\}$' || {
                    mkdir -p -m 700 "${HOME}/.cache" &&
                    exec startx 2>"${HOME}/.cache/xinitrc.log" 1>&2
                }
            fi;;
    esac
fi

hostname_to_color() {
    case ${1-} in
        *box) host_color=blue;;
        *gue) host_color=magenta;;
        *ils) host_color=cyan;;
        wol*) host_color=green;;
        *)    host_color=;;
    esac
}

if [ "${XDG_RUNTIME_DIR-}" ] && [ -z "${SSH_AUTH_SOCK+x}" ]
then
    SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
    export SSH_AUTH_SOCK
fi

# allow the title to be set using the `TITLE` variable (if supported);
# PROMPT_COMMAND is understood directly by Bash, whereas ~/.zshrc will evaluate
# the contents of PROMPT_COMMAND
case ${TERM} in
    *xterm*|*rxvt*|*konsole*)
        # note that the tilde replacement won't work if `HOME` has a trailing
        # slash, so don't put a trailing slash when setting `HOME` on Windows
        PROMPT_COMMAND='
            if [ "${TITLE+x}" ]; then          # if `TITLE` is set, use that
                printf "\033]0;%s\a" "${TITLE}"
            else
                case ${PWD} in
                    "${HOME}")   printf "\033]0;%s\a" "~";;
                    "${HOME}/"*) printf "\033]0;%s\a" "~${PWD#"${HOME}"}";;
                    *)           printf "\033]0;%s\a" "${PWD}";;
                esac
            fi
        '
esac
