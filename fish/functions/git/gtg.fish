function gtg
    git tag | fzf-tmux > /tmp/fzf-fix.tmp
    git checkout (cat /tmp/fzf-fix.tmp | sed "s/.* //")
end
