[[ $- != *i* ]] && return     # don't do anything if not running interactively

# Bash settings
HISTCONTROL=ignoreboth
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
    local weight=1
    local fg=30
    local user_bg=43
    local host_bg=47
    local host=$hostname
    local pwd_bg=44
    local continue='|'
    local select='?'
    local debug='|'

    # current Git branch
    local git_cmd='git 2>/dev/null rev-parse --abbrev-ref HEAD'
    local branch='$(b=`'"$git_cmd"'` && echo "($b) ")'

    # don't use bold in the Linux terminal because it looks bad
    [[ "$TERM" = linux ]] && weight=0

    # change color based on username
    [[ "$USER" = root ]] && { user_bg=45; prompt_fg=35; }

    # hide hostname on some systems and use color instead
    case "$HOSTNAME" in
        *-p8z68) host=":3"; host_bg=46;;
        *-g73jh) host=":3"; host_bg=42;;
        *-linux) host=":3"; host_bg=45;;
    esac
    local prompt_fg=$((host_bg - 10))

    # disable Git branch on Windows because it's slooow
    case "$OSTYPE" in cygwin|msys) branch=;; esac

    PS1=
    PS1+="${begin}$weight;$fg;$user_bg${end} $user "
    PS1+="${begin}$weight;$fg;$host_bg${end} $host "
    PS1+="${begin}$weight;$fg;$pwd_bg${end} $branch$pwd "
    PS1+="${begin}${end}\n"    # weird things can happen if newline is colored
    PS1+="${begin}$weight;$prompt_fg${end}$prompt${begin}${end} "
    PS2="${begin}$weight;$prompt_fg${end}$continue${begin}${end} "
    PS3="${begin}$weight;$prompt_fg${end}$select${begin}${end} "
    PS4="${begin}$weight;$prompt_fg${end}$debug${begin}${end} "

} && set_prompt; unset -f set_prompt

# allow the title to be set using the `TITLE` variable (if supported)
case "$TERM" in *xterm*|*rxvt*|*konsole*)
    # note that this won't work correctly if `HOME` has a trailing slash,
    # so don't put a trailing slash when setting `HOME` on Windows
    read -r -d '' PROMPT_COMMAND <<'EOF'
if [[ -z "${TITLE+x}" ]]; then          # if `TITLE` is unset
    # substitute home directory with tilde
    # can't use =~ here because MSYS Bash doesn't support it
    if [[ "$PWD" = "$HOME" ]]; then
        printf "\033]0;%s\a" "~"
    else
        if [[ "$PWD" = "$${PWD#$HOME/}" ]]; then
            printf "\033]0;%s\a" "$PWD"
        else
            printf "\033]0;%s\a" "~${PWD#$HOME}"
        fi
    fi
else
    printf "\033]0;%s\a" "$TITLE"
fi
EOF
;; esac

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

case "$OSTYPE" in
    cygwin|msys);;
    *)

        [ -f ~/.bashrc.local ] && . ~/.bashrc.local # source custom settings

        # gpg agent uses `pinentry` for password entry, so it's important for
        # gpg to be aware of the currently active tty
        export GPG_TTY=`tty`

        # environment modules
        if command -v modulecmd >/dev/null 2>&1; then
            module() { eval "`modulecmd sh "$@"`"; }
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
