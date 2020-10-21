source ~/.bash_aliases
source ~/.secrets.sh

# export PS1="\[\033[38;5;2m\]\W \\$\[$(tput sgr0)\]"

# export PS1="\[\033[1;32m\][\w]\n ==> \[\033[0m\]"
export PS1="\[\033[1;32m\][\w] λ \[\033[0m\]"
export PASSWORD_STORE_ENABLE_EXTENSIONS="true"
export INFOPATH=$INFOPATH:/home/guix/adrian/Documents/programming/lisp/info
# gpg-connect-agent \bye
# gpg-agent --daemon
gpgconf --launch gpg-agent
export SSH_AGENT_PID=""
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export PATH=$PATH:~/.npm/bin

## Vterm
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }

    vterm_prompt_end(){
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    PS1=$PS1'\[$(vterm_prompt_end)\]'
fi

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

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
