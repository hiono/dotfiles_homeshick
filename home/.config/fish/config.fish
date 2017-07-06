if status --is-interactive
  set -l IFS # this temporarily clears IFS, which disables the newline-splitting
  eval (keychain --eval --quiet -Q id_rsa | sed -e 's/\(.*\)=\(.*\); export .*$/set -x \1 \2/g')
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
    set -gx PATH $PATH ~/bin
end
set -x COMPOSE_HTTP_TIMEOUT 300
