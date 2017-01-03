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
    local pwd='\w'

    # foreground (fg):  30 to 37
    # background (bg):  40 to 47
    # weight:           0 [normal] or 1 [bold]
    local weight=1
    local primary_bg=47
    local user='\u'
    local hostname=' :3'
    local prompt='\$'

    # current Git branch
    local git_cmd='git 2>/dev/null rev-parse --abbrev-ref HEAD'
    local branch='$(b=`'"$git_cmd"'` && echo "($b) " || :)'

    # don't use bold in the Linux terminal because it looks bad
    if [ "$TERM" = linux ]; then
        weight=0
    fi

    # hide hostname on some systems and use color instead
    hostname_to_color "$HOSTNAME"
    unset -f hostname_to_color
    case $host_color in
        black)   primary_bg=40;;
        red)     primary_bg=41;;
        green)   primary_bg=42;;
        yellow)  primary_bg=43;;
        blue)    primary_bg=44;;
        magenta) primary_bg=45;;
        cyan)    primary_bg=46;;
        white)   primary_bg=47;;
        *)       hostname='@\h';;
    esac

    # change color based on username
    if [[ "$USER" = root ]]; then
        primary_bg=41
        user=
        hostname='@\h'
        prompt='#'
    fi

    # disable Git branch on Windows because it's slooow
    case $OSTYPE in
        cygwin) branch=; hostname='[cygwin]';;
        msys)   branch=; hostname='[msys]';;
    esac

    local primary_fg=$((primary_bg - 10))

    PS1=
    PS1+="${begin}$weight;30;$primary_bg${end} $user$hostname "
    PS1+="${begin}37;49${end} $branch"
    PS1+="${begin}$primary_fg${end}$pwd"
    PS1+="${begin}${end}\n"    # weird things can happen if newline is colored
    PS1+="${begin}$weight;$primary_fg${end}$prompt${begin}${end} "
    PS2="${begin}$weight;$primary_fg${end}|${begin}${end} "
    PS3="${begin}$weight;$primary_fg${end}?${begin}${end} "
    PS4="${begin}$weight;$primary_fg${end}|${begin}${end} "

} && set_prompt; unset -f set_prompt

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
