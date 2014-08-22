# don't do anything if not running interactively
[[ $- != *i* ]] && return

# Note: `uname` doesn't support `-o` on Git MSYS
SYSTEM=`uname`

# miscellaneous settings
HISTCONTROL=ignoreboth
HISTFILESIZE=2000
HISTSIZE=1000
shopt -s checkwinsize
shopt -s histappend

set_prompt() {

    # constants
    local begin='\[\e['
    local end='m\]'
    local user='\u'
    local hostname='\h'
    local pwd='\w'
    local prompt='\$'

    # foreground (fg):  30 to 37
    # background (bg):  40 to 47
    # weight:           0 [normal] or 1 [bold]
    local fg=30
    local user_bg=43
    local pwd_bg=44
    local continue='|'
    local select='?'
    local debug='|'

    # don't use bold in the Linux terminal
    local weight=1
    [[ "$TERM" = linux ]] && weight=0

    # change the prompt color based on username
    local prompt_fg=36
    [[ "$USER" = root ]] && prompt_fg=35

    # don't show hostname for some systems
    local user_suffix="@$hostname"
    case "$HOSTNAME" in
        *-p8z68) user_suffix=;;
        *-g73jh) user_suffix=;;
        *-linux) user_suffix=;;
    esac

    # change bg and text based on exit code;
    # we need to restore the exit code afterwards though
    local mid_bg='$(x=$?; [[ $x -eq 0 ]] && echo 42 || echo 41; exit $x)'
    local mid='$([[ $x -eq 0 ]] && echo ":3" || echo ":c")'

    # current Git branch
    local git_cmd='git 2>/dev/null rev-parse --abbrev-ref HEAD'
    local branch='$(b=`'"$git_cmd"'`&& echo " ($b)")'

    # disable interactivity on Windows due to slowness; perhaps using
    # PROMPT_COMMAND instead of a subshell would be faster for mid/mid_bg?
    case "$SYSTEM" in
        MINGW*|CYGWIN*)  mid_bg=42; mid=":3"; branch=;;
    esac

    PS1=
    PS1+="${begin}$weight;$fg;$user_bg${end} $user$user_suffix "
    PS1+="${begin}$weight;$fg;$mid_bg${end} $mid$branch "
    PS1+="${begin}$weight;$fg;$pwd_bg${end} $pwd "
    PS1+="${begin}${end}\n"    # weird things can happen if newline is colored
    PS1+="${begin}$weight;$prompt_fg${end}$prompt${begin}${end} "
    PS2="${begin}$weight;$prompt_fg${end}$continue${begin}${end} "
    PS3="${begin}$weight;$prompt_fg${end}$select${begin}${end} "
    PS4="${begin}$weight;$prompt_fg${end}$debug${begin}${end} "

} && set_prompt; unset -f set_prompt

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
