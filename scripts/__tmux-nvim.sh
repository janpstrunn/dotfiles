#!/bin/bash

source "$HOME/.env"

current_command=$(tmux display-message -p "#{pane_current_command}")

if [[ "$current_command" == "nvim" ]]; then
    tmux send-keys -t 0 ':echo fnamemodify(expand("'%:p'"), ":h")' C-m
    sleep 0.1

    dir=$(tmux capture-pane -pS - -E - | tail -n 1)

    if [[ -n "$dir" ]]; then
        tmux display-popup -h 90% -w 90% -E \
          "$SHELL -c 'cd \"$dir\" && exec $SHELL'"
    else
        echo "Could not retrieve the directory path."
    fi
else
    echo "Not in nvim."
fi
