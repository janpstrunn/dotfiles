#!/bin/bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__tmux-nvim.sh

source $HOME/.env

current_command=$(tmux display-message -p "#{pane_current_command}")

if [[ "$current_command" == "nvim" ]]; then
  tmux send-keys -t 0 ':echo fnamemodify(expand("'%:p'"), ":h")' C-m
  sleep 0.1

  dir=$(tmux capture-pane -pS - -E - | grep -oE '/[^[:space:]]+' | tail -n1)

  if [[ -n "$dir" ]]; then
    tmux display-popup -h 90% -w 90% -E \
      "$SHELL -c 'cd \"$dir\" && exec $SHELL'"
  else
    echo "Could not retrieve the directory path."
  fi
else
  tmux display-popup -d "#{pane_current_path}" -h 90% -w 90%
fi
