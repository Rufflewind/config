# don't do anything if not running interactively
[[ $- != *i* ]] && return

# miscellaneous settings
HISTCONTROL=ignoreboth
HISTFILESIZE=2000
HISTSIZE=1000
shopt -s checkwinsize
shopt -s histappend

# lots of programs get confused about urxvt so let's just pretend to be
# xterm-256color and hope that this doesn't break anything
export TERM=xterm-256color

# set the prompt
case "$HOSTNAME" in
    *-p8z68) PSCOLOR=32m;;
    *-g73jh) PSCOLOR=35m;;
    *-linux) PSCOLOR=36m;;
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
LS_IGNORES="-Intuser.* -INTUSER.*"      # ignore Windows system files
alias grep="grep --color=auto"
alias ls="ls --color=auto $LS_IGNORES"
unset LS_IGNORES
. ~/bin/gitaliases 2>/dev/null

SYSTEM=$(uname -o)
case "$SYSTEM" in
    Msys);;
    Cygwin);;
    *)
        # import variables for intel compilers
        [ -f ~/bin/intel-composer-envs ] && . ~/bin/intel-composer-envs

        # enable auto-completion if not already enabled
        if ! shopt -oq posix; then
            if [ -f /usr/share/bash-completion/bash_completion ]; then
                . /usr/share/bash-completion/bash_completion
            elif [ -f /etc/bash_completion ]; then
                . /etc/bash_completion
            fi
        fi
        ;;
esac
