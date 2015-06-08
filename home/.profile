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
then command_exists() { command >/dev/null 2>&1 -v "$1"; }
else command_exists() { type >/dev/null 2>&1 "$1"; }
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
    LESS=$LESS-qRS
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
        *cygwin*|*msys*)

            # do not use the `PATH` variable from Windows because it has a lot
            # of garbage that can severely reduce the performance of the shell
            PATH=/usr/local/bin:/usr/bin:/bin

            # we need `ping` because one of Rust's tests requires it, but it
            # MUST occur after other paths due to conflicts with, say, `find`.
            PATH=$PATH:/c/Windows/system32

            # to use MSYS with a different MinGW, set the `MINGW_PATH` to its
            # corresponding `bin` directory (do we also need to worry about
            # the libs as well?)
            [ -z "$EXTRA_PATH" ] || PATH=$EXTRA_PATH:$PATH
            unset EXTRA_PATH

            ;;
    esac

    case $OSTYPE in
        *cygwin*)

            # The CYGWIN variable is special: it must be set in
            # System Properties -> System variables, not here!
            #
            # CYGWIN=nodosfilewarning

            program_files="/cygdrive/c/Program Files"
            program_files_86="$program_files (x86)"

            # Lapack
            PATH=/usr/lib/lapack:$PATH

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

            : ${DISPLAY=:0}
            export DISPLAY

            ;;
        *msys*)

            # Python
            PATH=$PATH:/c/Python27

            # Git
            # - must contain `libcore` in its parent directory otherwise
            #   commands like `git pull` won't work.
            # - must NOT override Mingw commands or terminal will freeze up
            PATH=$PATH:/c/Git/bin

            ;;
        *)

            # find the latest Ruby version
            ruby_version_patt='s/.*\([0-9][0-9]*\.[0-9][0-9]*\).*/\1.0/'
            ruby_version=`{ ruby --version | sed "$ruby_version_patt";
                          } 2>/dev/null`
            if [ "$ruby_version" ]
            then PATH=$HOME/.gem/ruby/$ruby_version/bin:$PATH
            fi
            unset ruby_version ruby_version_patt

            PATH=$HOME/.cabal/bin:$PATH
            C_INCLUDE_PATH=$HOME/.local/include:$C_INCLUDE_PATH
            CPLUS_INCLUDE_PATH=$HOME/.local/include:$CPLUS_INCLUDE_PATH
            LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH
            LD_LIBRARY_PATH=$HOME/.local/lib:$LD_LIBRARY_PATH

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

            TEXMFHOME=$HOME/.config/texmf
            export TEXMFHOME

            alias pacman-upgrade="sudo pacman -Squyy --noprogressbar"
            alias pacaur-upgrade="pacaur -Squyy --noprogressbar"

            ;;
    esac

    # add ~/sbin and ~/bin/ to the PATH variable
    PATH=$HOME/.local/sbin:$HOME/.local/bin:$PATH
    export C_INCLUDE_PATH CPLUS_INCLUDE_PATH LD_LIBRARY_PATH LIBRARY_PATH PATH

    if [ "$TERM" = linux ]
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
        if [ "$RXVT_COMPAT" ]; then
            case $TERM in
                rxvt-unicode*)
                    TERM=xterm-16color
                    export TERM
                    ;;
            esac
        fi

esac

LS_COLORS='di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01'
LS_COLORS=$LS_COLORS':cd=40;33;01:or=40;31;01:su=37;41:sg=30;43'
LS_COLORS=$LS_COLORS':tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31'
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
    alias gitcar="git commit --amend --reset-author"
    alias gitcanr="git commit --amend --no-edit --reset-author"
    alias gitd="git diff"
    alias gitdn="git diff --no-index"
    alias gitdc="git diff --cached"
    alias gitf="git fetch"
    alias gitfa="git fetch --all -p"
    alias gith="git checkout"
    alias githb="git checkout -B"
    alias gitka="gitk --all"
    alias gitl="git log"
    alias gito="git remote"
    alias gitn="git branch"
    alias gitu="git pull"
    alias gitur="git pull -r"
    alias gitp="git push"
    alias gitr="git reset"
    alias gits="git status"
    alias gitt="git stash"
    alias grep="grep --color=auto"
    alias R="R --no-save"
    # hide Windows system files
    alias ls="ls --color=auto '-Intuser.*' '-INTUSER.*'"
    alias sshfs="sshfs -o ssh_command='ssh -S none'"
    alias wanip='dig +short myip.opendns.com @resolver1.opendns.com'
fi

hist() {
    grep -a --color=auto "$@" "$HOME/.histfile"
}

randomstring() {
    enc=${1-base64}
    n=${2-60}
    head -c "$n" /dev/urandom | encode -s -w0 -u "$enc"
    echo
}

uppercase() {
    tr "[[:lower:]]" "[[:upper:]]"
}

lowercase() {
    tr "[[:upper:]]" "[[:lower:]]"
}

# show the entire history of a Git repo as a colored tree in the terminal
gitv() {
    git log "$@" --all --color --date=short --full-history --graph  \
        --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%ad %s"
}

gittd() {
    git stash && "$@" && git stash pop
}

# undo the effect of Alt + SysRq + r
reraw() {
    sudo kbd_mode -s -C /dev/tty1
}

# bash/bsh-specific
if [ "$BASH_VERSION$ZSH_VERSION" ]
then
    HISTCONTROL=erasedups:ignoreboth
    HISTSIZE=1000
    SAVEHIST=1000
fi

# cleanup
if [ "$unset_ostype" ]
then unset unset_ostype OSTYPE
fi
unset command_exists

# start X server if needed
if [ "$1" != rc ]
then
    case $OSTYPE in
        *cygwin*|*msys*);;
        *)
            if [ -z "$DISPLAY" ] && [ "`fgconsole 2>/dev/null`" = 1 ] &&
               command >/dev/null 2>&1 -v startx &&
               command >/dev/null 2>&1 -v xset
            then ps -A -o comm | grep >/dev/null 2>&1 '^X\(org\)\{0,1\}$' ||
                 exec startx
            fi;;
    esac
fi

hostname_to_color() {
    case $1 in
        *nux)      host_color=blue;;
        *gue)      host_color=magenta;;
        *aws|*3jh) host_color=green;;
        *)         host_color=;;
    esac
}

if [ -f "$HOME/.ssh/agent" ]
then . "$HOME/.ssh/agent" >/dev/null
fi
