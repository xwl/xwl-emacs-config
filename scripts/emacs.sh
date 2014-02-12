# alias ec='LC_ALL=zh_CN.utf8 TERM=xterm-256color emacsclient -t -e "(command-execute (kbd \"C-c m s\"))"'
alias ec='LC_ALL=zh_CN.utf8 TERM=xterm-256color emacsclient -t'
alias ecui='LC_ALL=zh_CN.utf8 TERM=xterm-256color emacsclient -c'

export ALTERNATE_EDITOR=~/.emacs.d/scripts/emacs-daemon
export PATH=$PATH:~/.emacs.d/scripts/git_util:~/.emacs.d/scripts/http_proxy4git/connect

if [[ `uname -s` != "Darwin" ]]; then
    if [[ $http_proxy = "" ]]; then
        echo "Empty http_proxy!"
        exit
    fi

    connect > /dev/null 2>&1 && {
        export GIT_SSH=~/.emacs.d/scripts/http_proxy4git/socks5proxy_ssh;
        git config --global core.gitproxy ~/.emacs.d/scripts/http_proxy4git/socks5proxy
        git config --global http.proxy $http_proxy
    }
fi

# ,----
# | git 
# `----

function git_local ()
{
    (git branch > /dev/null 2>&1) && (git branch | grep ^\* | sed 's/* //')
}

function git_remote ()
{
    LC_ALL=C git branch -vv | grep ^\* | cut -d[ -f2 | cut -d: -f1 | cut -d] -f1
}

function git_remote_short ()
{
    git_remote | sed 's:origin/::'
}

function review ()
{
    git push origin $(echo HEAD:refs/for/$(git_remote_short))
}

# Sort branch by commit date.
function git_branch()
{
    for i in `git for-each-ref --sort=-committerdate --format='%(refname:short)' refs/heads/`
    do
        printf "  %-30s %s\n" $i "`git log -1 --pretty=format:'%h [%C(blue)%cr%Creset] %s' $i`"
    done
}
