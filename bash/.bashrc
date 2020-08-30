source ~/.bash_aliases
source ~/.secrets.sh

# export PS1="\[\033[38;5;2m\]\W \\$\[$(tput sgr0)\]"

# export PS1="\[\033[1;32m\][\w]\n ==> \[\033[0m\]"
export PS1="\[\033[1;32m\][\w] λ \[\033[0m\]"
export PASSWORD_STORE_ENABLE_EXTENSIONS="true"
export INFOPATH=$INFOPATH:/home/guix/adrian/Documents/programming/lisp/info

# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
source /etc/bashrc
