alias ec='emacsclient -t -e "(command-execute (kbd \"C-c m s\"))"'

export ALTERNATE_EDITOR=~/.emacs.d/scripts/emacs-daemon

if [[ `uname -s` != "Darwin" ]]; then
    export GIT_SSH=~/.emacs.d/scripts/http_proxy4git/socks5proxy_ssh
fi

export PATH=$PATH:~/.emacs.d/scripts/git_util


# ,----
# | git 
# `----

function git_local ()
{
    (git branch > /dev/null 2>&1) && (git branch | grep ^\* | sed 's/* //')
}

function git_remote ()
{
    LC_ALL=C git branch -vv | grep ^\* | cut -d[ -f2 | cut -d] -d\: -f1
}

function git_remote_short ()
{
    git_remote | sed 's:origin/::'
}

function review ()
{
    git push origin $(echo HEAD:refs/for/$(git_remote_short))
}
