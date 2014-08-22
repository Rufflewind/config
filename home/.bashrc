# don't do anything if not running interactively
[[ $- != *i* ]] && return

# miscellaneous settings
HISTCONTROL=ignoreboth
HISTFILESIZE=2000
HISTSIZE=1000
shopt -s checkwinsize
shopt -s histappend

set_prompt() {
    local pscolor prompt_color
    case "$HOSTNAME" in
        # *-p8z68)   pscolor=42m;;
        # *-g73jh)   pscolor=45m;;
        # *-linux)   pscolor=46m;;
        # gateway-*) pscolor=41m;;
        # dev-*)     pscolor=44m;;
        *)  pscolor='43m';
            pshostname='@\h';;
    esac
    local prompt_color='3$([[ "\u" = root ]] && echo 5 || echo 6)'

    PS1=
    PS1+='\[\e[1;30;'"$pscolor"'\] \u'"$pshostname "
    PS1+='\[\e[1;30;4$(x=$?; [[ $x -eq 0 ]] && echo 2 || echo 1; exit $x)m\]'
    PS1+=' :$([[ $? -eq 0 ]] && echo 3 || echo C) '

    # current Git branch
    PS1+='$(b=`git 2>/dev/null rev-parse --abbrev-ref HEAD` && echo "($b) ")'

    # working dir
    PS1+='\[\e[1;30;44m\] \w '

    PS1+='\[\e[m\]\n'                 # weird things happen if \n gets colored
    PS1+='\[\e[1;'"$prompt_color"'m\]\$ \[\e[m\]'
    PS2='\[\e[1;'"$prompt_color"'m\]… \[\e[m\]'
    PS3='\[\e[1;'"$prompt_color"'m\]? \[\e[m\]'
    PS3='\[\e[1;'"$prompt_color"'m\]> \[\e[m\]'
}
set_prompt
unset -f set_prompt

# set the title (if supported)
# note: to override this, set the `TITLE` variable
have_title=
case "$TERM" in
    *xterm*)   have_title=t;;
    *rxvt*)    have_title=t;;
    *konsole*) have_title=t;;
esac
if [[ $have_title ]]; then
    # note that this won't work correctly if `HOME` has a trailing slash, so
    # don't put a trailing slash when setting `HOME` on Windows
    read -r -d '' PROMPT_COMMAND <<'EOF'
if [[ -z "${TITLE+x}" ]]; then       # if `TITLE` is unset
    # substitute home directory with tilde
    # can't use =~ here because MSYS Bash doesn't support it
    if [[ "$PWD" = "$HOME" ]]; then
        printf "\033]0;%s\a" "~"
    else
        __PWD_WITHOUT_HOME=${PWD#$HOME/}
        if [[ "$PWD" = "$__PWD_WITHOUT_HOME" ]]; then
            printf "\033]0;%s\a" "$PWD"
        else
            printf "\033]0;%s\a" "~${PWD#$HOME}"
        fi
        unset __PWD_WITHOUT_HOME
    fi
else
    printf "\033]0;%s\a" "$TITLE"
fi
EOF
fi
unset have_title

# aliases
alias ls="ls --color=auto -Intuser.* -INTUSER.*" # ignore Windows system files
alias grep="grep --color=auto"
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

# Note: `uname` doesn't support `-o` on Git MSYS
SYSTEM=`uname`
case "$SYSTEM" in
    MINGW*);;
    CYGWIN*);;
    *)

        # source custom settings
        [ -f ~/.bashrc.local ] && . ~/.bashrc.local

        # gpg agent uses `pinentry` for password entry, so it's important for
        # gpg to be aware of the currently active tty
        export GPG_TTY=`tty`

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

:                                       # make sure exit code is zero
