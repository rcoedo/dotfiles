function __fuzzy_tmux_kill
    tmux list-sessions -F "#{session_name}" | __fuzzy_find | read -l session
    if test -n "$session"
        tmux kill-session -t $session
    end
end
