alias em='emacsclient -nw -a ""'
alias kille='emacsclient -e "(kill-emacs)"'
alias docker-rmexit='docker ps -a -q -f "status=exited" | xargs --no-run-if-empty docker rm -v'
alias docker-rminone='docker images -q -f "dangling=true" | xargs --no-run-if-empty docker rmi'
