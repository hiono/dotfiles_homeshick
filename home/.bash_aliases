alias em='emacsclient -nw -a "emacs"'
alias kille='emacsclient -e "(kill-emacs)"'
alias drmst='docker rm $(docker ps -a --filter "status=exited" -q)'
