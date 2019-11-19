source ~/.bash_aliases
source ~/.secrets.sh

# export PS1="\[\033[38;5;2m\]\W \\$\[$(tput sgr0)\]"

# export PS1="\[\033[1;32m\][\w]\n ==> \[\033[0m\]"
export PS1="\[\033[1;32m\][\w] λ \[\033[0m\]"

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

# Adjust the prompt depending on whether we're in 'guix environment'.
# if [ -n "$GUIX_ENVIRONMENT" ]
# then
#     PS1='\u@\h \w [env]\$ '
# else
#     PS1='\u@\h \w\$ '
# fi
# alias ls='ls -p --color=auto'
# alias ll='ls -l'
# alias grep='grep --color=auto'


# # Disable completion if run from emacs.
# if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) &&\
#           -f /etc/bash_completion ]]; then
#     . /etc/bash_completion
# fi

# if [ "$TERM" == "dumb" ]; then
#     # export PS1='\n[\u@\h:\w]\$ '
# else 
#    # Src - source bashrc
#    Src() {
#        source ~/.bashrc
#    }

#    emacs-daemon-workspace() {
#        "emacs --daemon=$1"
#    }

#    # Functions with fzf ## (many from https://github.com/junegunn/fzf/wiki/examples)

#    # fd - cd to selected directory
#    fd() {
#        local dir
#        dir=$(find ${1:-.} -path '*/\.*' -prune \
#                   -o -type d -print 2> /dev/null | fzf +m) &&
#            cd "$dir"
#    }

#    # fda - including hidden directories
#    fda() {
#        local dir
#        dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
#    }

#    # fh - repeat history
#    fh() {
#        eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
#    }

#    # fkill - kill processes
#    fkill() {
#        local pid 
#        if [ "$UID" != "0" ]; then
#            pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
#        else
#            pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
#        fi  

#        if [ "x$pid" != "x" ]
#        then
#            echo $pid | xargs kill -${1:-9}
#        fi  
#    }

#    # fbr - checkout git branch (including remote branches)
#    fbr() {
#        local branches branch
#        branches=$(git branch --all | grep -v HEAD) &&
#            branch=$(echo "$branches" |
#                         fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
#            git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
#    }

#    [ -f ~/.fzf.bash ] && source ~/.fzf.bash
# fi
# # export LUA_PATH='/home/adrian/.luarocks/share/lua/5.2/?.lua;/home/adrian/.luarocks/share/lua/5.2/?/init.lua;/nix/store/26gb452y4z8yynjhz2k6cbln9yqn5zmq-luarocks-2.4.4/share/lua/5.2/?.lua;/nix/store/26gb452y4z8yynjhz2k6cbln9yqn5zmq-luarocks-2.4.4/share/lua/5.2/?/init.lua'
# # export LUA_CPATH='/home/adrian/.luarocks/lib/lua/5.2/?.so;/nix/store/26gb452y4z8yynjhz2k6cbln9yqn5zmq-luarocks-2.4.4/lib/lua/5.2/?.so;/usr/lib/lua/5.2/?.so;/usr/lib/lua/5.2/loadall.so;./?.so'
