# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# My alias'
alias nocaps="setxkbmap -option ctrl:nocaps"
alias caps="setxkbmap -option -option capslock:caps"
alias m='more'
alias g='grep'
alias win-office='rdesktop -g 1200x800 -k tr 194.27.61.118'
alias ssh-office='ssh -l askin -p 3389 95.183.152.25'
alias google-dns='echo "nameserver 8.8.8.8" | sudo tee /etc/resolv.conf > /dev/null'

alias .="cd .."
alias ..="cd ../.."
alias ...="cd ../../.."
alias ....="cd ../../../.."
alias .....="cd ../../../../.."
alias ......="cd ../../../../.."

# colorfull stuff
alias diff='colordiff'
alias dmesg='dmesg | ccze -A'
psc () { /bin/ps $@ | ccze -A; }
tail () { /usr/bin/tail $@ | ccze -A; }

# My Functions
function svndiff() {
    svn diff "${@}" | colordiff
}

# create temp directory
alias tf="source ~/.bash/tf"