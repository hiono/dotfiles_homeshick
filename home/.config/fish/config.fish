if status --is-interactive
  set -l IFS # this temporarily clears IFS, which disables the newline-splitting
  eval (keychain --eval --quiet -Q id_rsa | sed -e 's/\(.*\)=\(.*\); export .*$/set -x \1 \2/g')
end
set -x COMPOSE_HTTP_TIMEOUT 300
if status --is-login
    set -x PATH $PATH ~/bin
end
