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