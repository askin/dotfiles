# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# 31 red
# 32 green
# 33 yellow
sc="\[\033[0;33;60m\]"
hn="\[\033[0;32;60m\]"
un="\[\033[0;31;60m\]"
wc="\[\033[0m\]"

if [ `whoami` == 'root' ]; then
    dl='#'
else
    dl='$'
fi

# PS1=${un}'\u'${sc}@${hn}'\h':${sc}'~$ '${wc}
PS1="${hn}[${sc}${un}\u${sc}@${hn}\h${un}:${sc}${sc}\w${hn}]${un}${dl} ${wc}"

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
        ;;
    *)
        ;;
esac

# enable programmable completion feature
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# extra configurations
[ -f ~/.shell/extra.sh ] && source ~/.shell/extra.sh

# include aliases
source ~/.shell/aliases.sh

# include variables
source ~/.shell/variables.sh

# solorized theme
eval `dircolors ~/.shell/dircolors-solorized/dircolors.256dark`
