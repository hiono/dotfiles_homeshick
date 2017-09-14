if status --is-interactive
    set -l IFS # this temporarily clears IFS, which disables the newline-splitting
    eval (ssh-agent -c) > /dev/null
end
if status --is-login
    # add PATH
    set -x PATH $PATH ~/bin
    # set -x PATH $PATH ~/bin ~/.emacs.d/**/el-get/rtags/bin
    # # for rtags
    # which rdm > /dev/null
    # if test $status -eq 0
    #     pgrep rdm > /dev/null
    #     if test $status -ne 0
    #         rdm --daemon
    #     end
    # end
end
set -x COMPOSE_HTTP_TIMEOUT 300
