. ~/.profile rc

# miscellaneous settings
HISTFILE=~/.histfile
unsetopt beep
bindkey -e
zstyle :compinstall filename ~/.zshrc
autoload -Uz compinit
compinit
setopt HIST_IGNORE_DUPS HIST_IGNORE_SPACE

# prevent zsh from being too zealous with Alt+Backspace
# (must occur before zsh-syntax-highlighting)
autoload -U select-word-style
select-word-style bash

# allow comments in interactive mode
setopt interactivecomments

# enable syntax highlighting
# (must occur after select-word-style bash)
if [ -f /usr/share/zsh/plugins/\
zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]
then  . /usr/share/zsh/plugins/\
zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# set the prompt
autoload -U promptinit
promptinit
rf_set_prompt() {
    # Generic colour fade-bar prompt theme from bashprompt
    # Created by Jim Foltz <aa204@acorn.net>
    # Changed by Spidey 08/06
    # Converted to zsh prompt theme by <adam@spiers.net>
    # Heavily modified by Rufflewind <rf@rufflewind.com>

    local weight="%B"
    local primary=yellow
    local user='%n'
    local hostname=
    local prompt_char='\$'

    # don't use bold in the Linux terminal because it looks bad
    if [ "$TERM" = linux ]; then
        weight=
    fi

    # hide hostname on some systems and use color instead
    hostname_to_color "`hostname`"
    unset -f hostname_to_color
    if [ "$host_color" ]; then
        primary=$host_color
    else
        hostname="@%m"
    fi

    # change color based on username
    if [ "$USER" = root ]; then
        primary=red
        user=
        hostname="@%m"
        prompt_char='#'
    fi

    local -A schars
    autoload -Uz prompt_special_chars
    prompt_special_chars

    # current Git branch
    add-zsh-hook precmd rf_precmd_vcs_info
    rf_precmd_vcs_info() {
        local -a git_cmd
        git_cmd=(git rev-parse --abbrev-ref HEAD)
        rf_vcs_branch=$(b=$($git_cmd 2>/dev/null) && echo "($b) " || :)
    }

    setopt prompt_subst
    PROMPT="%F{$primary}${weight}%K{$primary} "
    PROMPT+="%F{black}%K{$primary}$user$hostname%b%F{$primary}%k"
    PROMPT+="%K{$primary} %K{white}"
    PROMPT+="%F{black}%B %D{%a %H:%M:%S} %k "
    PROMPT+="%F{white}\${rf_vcs_branch}%F{$primary}%~$prompt_newline"
    PROMPT+="%F{$primary}%k%B$prompt_char%b%k%f "
    PS2="%F{$primary}%k%B|%b%k%f "
    PS3="%F{$primary}%k%B?%b%k%f "
    PS4="%F{$primary}%k%B|%b%k%f "

} && rf_set_prompt; unset -f rf_set_prompt

precmd() {
    eval "${PROMPT_COMMAND-}"
}

# remember last visited directories
DIRSTACKFILE="$HOME/.cache/zsh/dirs"
mkdir -p "$HOME/.cache/zsh"
if [ -f "$DIRSTACKFILE" ] && [ $#dirstack -eq 0 ]; then
    dirstack=(${(f)"$(< $DIRSTACKFILE)"})
    # [ -d "$dirstack[1]" ] && cd "$dirstack[1]"
fi
chpwd() { print >"$DIRSTACKFILE" -l "$PWD" ${(u)dirstack} }
DIRSTACKSIZE=20
setopt autopushd pushdsilent pushdtohome pushdignoredups pushdminus

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key
key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}
# set up keys accordingly
[ "${key[Home]}" ] && bindkey "${key[Home]}" beginning-of-line
[ "${key[End]}" ] && bindkey "${key[End]}" end-of-line
[ "${key[Insert]}" ] && bindkey "${key[Insert]}" overwrite-mode
[ "${key[Delete]}" ] && bindkey "${key[Delete]}" delete-char
[ "${key[Up]}" ] && bindkey "${key[Up]}" up-line-or-history
[ "${key[Down]}" ] && bindkey "${key[Down]}" down-line-or-history
[ "${key[Left]}" ] && bindkey "${key[Left]}" backward-char
[ "${key[Right]}" ] && bindkey "${key[Right]}" forward-char
[ "${key[PageUp]}" ] && bindkey "${key[PageUp]}" beginning-of-buffer-or-history
[ "${key[PageDown]}" ] && bindkey "${key[PageDown]}" end-of-buffer-or-history
# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if [ "${terminfo[smkx]}" ] && [ "${terminfo[rmkx]}" ]; then
    zle-line-init   () { echoti smkx; }
    zle-line-finish () { echoti rmkx; }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S\nmem\t%MM'

fpath=(~/.rustup/toolchains/*/share/zsh/site-functions/(N) $fpath)

# OPAM configuration
. "${HOME}/.opam/opam-init/init.zsh" >/dev/null 2>/dev/null || true
