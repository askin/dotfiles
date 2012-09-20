# PATH
export PATH=$PATH:/home/home/bin

export ORACLE_HOME=/opt/instantclient_11_2
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ORACLE_HOME
export TNS_ADMIN=/home/askin/Oracle/

# aliases
# My Aliases
alias maia="ssh -l smcard 193.140.151.8"
alias pssh="parallel-ssh -h "
alias tf="source /home/askin/.bash/tf"
alias idefix='ssh -l askin idefix'
alias smartguard='ssh -l oracle smartguard'
alias oburix='ssh -l askin oburix -p 3389'
alias sg='ssh -l root'

# rename tab
alias gr='guake -r'

# ipythonx
alias ipythonx='ipython qtconsole'
