# don't do anything if not running interactively
case $- in
    *i*) ;;                             # interactive
    *) return;;                         # non-interactive
esac

# load `~/.profile` in nonlogin mode
if [ -f ~/.profile ]
then
    . ~/.profile rc
fi

# Bash settings
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
    local branch='$(b=`'"$git_cmd"'` && echo "($b) " || :)'

    # don't use bold in the Linux terminal because it looks bad
    [[ "$TERM" = linux ]] && weight=0

    # change color based on username
    [[ "$USER" = root ]] && { user_bg=41; prompt_fg=35; }

    # hide hostname on some systems and use color instead
    case $HOSTNAME in
        *-p8z68) host=":3"; host_bg=46;;
        *-g73jh) host=":3"; host_bg=42;;
        *-linux) host=":3"; host_bg=45;;
    esac
    case $OSTYPE in
        msys) host=":\\"; host_bg=47;;
    esac
    local prompt_fg=$((host_bg - 10))

    # disable Git branch on Windows because it's slooow
    case $OSTYPE in cygwin|msys) branch=;; esac

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
case $TERM in *xterm*|*rxvt*|*konsole*)
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

# enable auto-completion if not already enabled
case $OSTYPE in
    cygwin|msys);;
    *)
        if ! shopt -oq posix
        then
            if [ -f /usr/share/bash-completion/bash_completion ]
            then
                . /usr/share/bash-completion/bash_completion
            elif [ -f /etc/bash_completion ]
            then
                . /etc/bash_completion
            fi
        fi
        ;;
esac

if [ -f ~/.bashrc_overrides ]
then
    . ~/.bashrc_overrides
fi
