alias ec='emacsclient -t -e "(command-execute (kbd \"C-c m s\"))"'

export ALTERNATE_EDITOR=~/.emacs.d/scripts/emacs-daemon

if [[ `uname -s` != "Darwin" ]]; then
    export GIT_SSH=~/.emacs.d/scripts/http_proxy4git/socks5proxy_ssh
fi

export PATH=$PATH:~/.emacs.d/scripts/git_util
