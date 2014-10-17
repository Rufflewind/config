. ~/.profile rc

# miscellaneous settings
HISTFILE=~/.histfile
unsetopt beep
bindkey -e
zstyle :compinstall filename ~/.zshrc
autoload -Uz compinit
compinit

# enable syntax highlighting
if [ -f /usr/share/zsh/plugins/\
zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]
then  . /usr/share/zsh/plugins/\
zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# set the prompt
autoload -U promptinit
promptinit
prompt fade green
