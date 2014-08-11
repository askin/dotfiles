# bash complete script for report_waiting_packages command

_report_waiting_packages() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="FAX IVR VM"

    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
}

complete -F _report_waiting_packages report-waiting-packages
