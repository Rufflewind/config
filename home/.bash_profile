if [ -f ~/.profile ]
then
    . ~/.profile                        # note: might not return
fi

if [ -f ~/.bashrc ]
then
    . ~/.bashrc
fi
