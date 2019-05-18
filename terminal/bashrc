source ~/.bash_aliases

#export PS1="\W \$"

if [ "$TERM" == "dumb" ]; then
    export PS1='\n[\u@\h:\w]\$ '
else 
   # Src - source bashrc
   Src() {
       source ~/.bashrc
   }

   emacs-daemon-workspace() {
       "emacs --daemon=$1"
   }

   # Functions with fzf ## (many from https://github.com/junegunn/fzf/wiki/examples)

   # fd - cd to selected directory
   fd() {
       local dir
       dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
           cd "$dir"
   }

   # fda - including hidden directories
   fda() {
       local dir
       dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
   }

   # fh - repeat history
   fh() {
       eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
   }

   # fkill - kill processes
   fkill() {
       local pid 
       if [ "$UID" != "0" ]; then
           pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
       else
           pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
       fi  

       if [ "x$pid" != "x" ]
       then
           echo $pid | xargs kill -${1:-9}
       fi  
   }

   # fbr - checkout git branch (including remote branches)
   fbr() {
       local branches branch
       branches=$(git branch --all | grep -v HEAD) &&
           branch=$(echo "$branches" |
                        fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
           git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
   }

   [ -f ~/.fzf.bash ] && source ~/.fzf.bash
fi
