export EDITOR='LC_ALL=zh_CN.utf8 TERM=xterm-256color emacsclient -t'

# alias ec='LC_ALL=zh_CN.utf8 TERM=xterm-256color emacsclient -t -e "(command-execute (kbd \"C-c m s\"))"'
alias ec=$EDITOR
alias ecui='LC_ALL=zh_CN.utf8 TERM=xterm-256color emacsclient -n'
alias ecui2='LC_ALL=zh_CN.utf8 TERM=xterm-256color emacsclient -c'

export ALTERNATE_EDITOR=~/.emacs.d/scripts/emacs-daemon

export PATH=$PATH:~/.emacs.d/scripts/git_util:~/.emacs.d/scripts/http_proxy4git/connect

if [[ $(uname -s) != "Darwin" ]]; then
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

git_local ()
{
    (git branch > /dev/null 2>&1) && (git branch | grep ^\* | sed 's/* //')
}

git_remote ()
{
    LC_ALL=C git branch -vv | grep ^\* | cut -d[ -f2 | cut -d: -f1 | cut -d] -f1
}

git_remote_short ()
{
    git_remote | sed 's:origin/::'
}

review ()
{
    git push origin $(echo HEAD:refs/for/$(git_remote_short))
}

# Sort branch by commit date.
git_branch()
{
    for i in $(git for-each-ref --sort=-committerdate --format='%(refname:short)' refs/heads/)
    do
        printf "  %-30s %s\n" $i "$(git log -1 --pretty=format:'%h [%C(blue)%cr%Creset] %s' $i)"
    done
}

git_current_branch()
{
    local result
    result=$(__git_ps1 "%s" 2>/dev/null)

    [[ $result != "" ]] && echo :$result
}

repo_rev ()
{
    [[ $(pwd) = */android* ]] || return

    local result
    result=$((repo info ./ |grep "Manifest branch" | cut -d: -f2 | cut -d' ' -f2 | cut -d'/' -f 3) 2>/dev/null)

    [[ $result != "" ]] && echo :$result
}

repo_project ()
{
    [[ $(pwd) = */android* ]] || return

    local result
    result=$((repo info ./ |grep "Project:" | cut -d' ' -f2) 2>/dev/null)

    [[ $result != "" ]] && echo :$result
}

export PS1='\u@\h\[\e[m\]\[\e[0;33m\]$(repo_rev)\[\e[m\]\[\e[1;36m\]$(repo_project)\[\e[m\]\[\e[1;35m\]$(git_current_branch)\[\e[m\]:\W \$ '
