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

    local primary=yellow
    local hostname="@%m"
    local weight="%B"

    # don't use bold in the Linux terminal because it looks bad
    [ "$TERM" = linux ] && weight=

    # hide hostname on some systems and use color instead
    case `hostname` in
        *-linux)  hostname=; primary=blue;;
        wolfpaws) hostname=; primary=green;;
    esac

    # change color based on username
    [ "$USER" = root ] && primary=red

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
    PROMPT="%F{$primary}${weight}%K{$primary}$schars[261]$schars[260]"
    PROMPT+="%F{black}%K{$primary}%n$hostname%b%F{$primary}%k"
    PROMPT+="$schars[333]$schars[262]$schars[261]$schars[260]"
    PROMPT+="%F{white}%k%B %D{%a %H:%M:%S} "
    PROMPT+="\${rf_vcs_branch}%F{$primary}%~/$prompt_newline"
    PROMPT+="%F{$primary}%k%B\\\$%b%k%f "
    PS2="%F{$primary}%k%B|%b%k%f "
    PS3="%F{$primary}%k%B?%b%k%f "
    PS4="%F{$primary}%k%B|%b%k%f "

} && rf_set_prompt; unset -f rf_set_prompt

# remember last visited directories
DIRSTACKFILE="$HOME/.cache/zsh/dirs"
mkdir -p "$HOME/.cache/zsh"
if [ -f "$DIRSTACKFILE" ] && [ $#dirstack -eq 0 ]; then
    dirstack=(${(f)"$(< $DIRSTACKFILE)"})
    [ -d "$dirstack[1]" ] && cd "$dirstack[1]"
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
# setup key accordingly
[[ -n "${key[Home]}"   ]] && bindkey "${key[Home]}"   beginning-of-line
[[ -n "${key[End]}"    ]] && bindkey "${key[End]}"    end-of-line
[[ -n "${key[Insert]}" ]] && bindkey "${key[Insert]}" overwrite-mode
[[ -n "${key[Delete]}" ]] && bindkey "${key[Delete]}" delete-char
[[ -n "${key[Up]}"     ]] && bindkey "${key[Up]}"     up-line-or-history
[[ -n "${key[Down]}"   ]] && bindkey "${key[Down]}"   down-line-or-history
[[ -n "${key[Left]}"   ]] && bindkey "${key[Left]}"   backward-char
[[ -n "${key[Right]}"  ]] && bindkey "${key[Right]}"  forward-char
# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
zle-line-init()   { echoti smkx }
zle-line-finish() { echoti rmkx }
if [[ -n ${terminfo[smkx]} ]] && [[ -n ${terminfo[rmkx]} ]]; then
    zle -N zle-line-init
    zle -N zle-line-finish
fi

TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S\nmem\t%MM'
