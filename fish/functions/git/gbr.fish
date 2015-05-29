function gbr
    git branch --all | grep -v HEAD | fzf-tmux > /tmp/fzf-fix.tmp
    git checkout (cat /tmp/fzf-fix.tmp | sed "s/.* //" | sed "s#remotes/[^/]*/##")
end
