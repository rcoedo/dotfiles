function __fuzzy_tmux_attach
    tmux list-sessions -F "#{session_name}" | __fuzzy_find | read -l session
    if test -n "$session"
        tmux attach -t $session
    end
end
