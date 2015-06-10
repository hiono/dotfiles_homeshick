alias em='emacsclient -nw -a ""'
alias kille='emacsclient -e "(kill-emacs)"'
alias drmst='docker rm $(docker ps -a --filter "status=exited" -q)'
