# don't do anything if not running interactively
[[ $- != *i* ]] && return

# miscellaneous settings
HISTCONTROL=ignoreboth
HISTFILESIZE=2000
HISTSIZE=1000
shopt -s checkwinsize
shopt -s histappend

# set the prompt
case "$HOSTNAME" in
    *-p8z68) PSCOLOR=32m;;
    *-g73jh) PSCOLOR=35m;;
    *-linux) PSCOLOR=36m;;
    gateway-*) PSCOLOR=31m;;
    dev-*) PSCOLOR=34m;;
    *) PSCOLOR=33m;;
esac
PS1="\[\e[0;$PSCOLOR\]\u\[\e[0;33m\] :3 \[\e[1;34m\]\w\[\e[m\]\n"
unset PSCOLOR

# set the title (if supported)
# note: to override this, set the `TITLE` variable
HAVE_TITLE=
case "$TERM" in
    *xterm*)   HAVE_TITLE=t;;
    *rxvt*)    HAVE_TITLE=t;;
    *konsole*) HAVE_TITLE=t;;
esac
if [ $HAVE_TITLE ]; then
    # note that this won't work correctly if `HOME` has a trailing slash, so
    # don't put a trailing slash when setting `HOME` on Windows
    read -r -d '' PROMPT_COMMAND <<'EOF'
    if [ -z "${TITLE+x}" ]              # if `TITLE` is unset
    then
        # substitute home directory with tilde
        if [[ "$PWD" =~ ^"$HOME"(/|$) ]]
        then printf "\033]0;%s\a" "~${PWD#$HOME}"
        else printf "\033]0;%s\a" "$PWD"
        fi
    else
        printf "\033]0;%s\a" "$TITLE"
    fi
EOF
fi
unset HAVE_TITLE

# aliases
alias ls="ls --color=auto -Intuser.* -INTUSER.*" # ignore Windows system files
alias grep="grep --color=auto"
alias gita="git add"
alias gitau="git add -u"
alias gitc="git commit"
alias gitd="git diff"
alias gitdc="git diff --cached"
alias gitl="git log"
alias gitu="git pull"
alias gitp="git push"
alias gits="git status"

SYSTEM=$(uname -o)
case "$SYSTEM" in
    Msys);;
    Cygwin);;
    *)

        # source custom settings
        [ -f ~/.bashrc.local ] && . ~/.bashrc.local

        # environment modules
        if command -v modulecmd >/dev/null 2>&1; then
            module() {
                eval "`modulecmd sh "$@"`"
            }
            module use /etc/environment-modules.d
            export MODULERCFILE=/etc/environment-modules
        fi

        # enable auto-completion if not already enabled
        if ! shopt -oq posix; then
            if [ -f /usr/share/bash-completion/bash_completion ]; then
                . /usr/share/bash-completion/bash_completion
            elif [ -f /etc/bash_completion ]; then
                . /etc/bash_completion
            fi
        fi

        # some systems don't know urxvt, so let's pretend to be xterm;
        # however, don't fake as `xterm-256color` because emacs colors would
        # look weird
        if [ "$RXVT_COMPAT" ]; then
            case "$TERM" in
                rxvt-unicode*) export TERM=xterm-16color;;
            esac
        fi

        ;;
esac
