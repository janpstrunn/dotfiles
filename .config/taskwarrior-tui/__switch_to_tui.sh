#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/.config/taskwarrior-tui/__switch_to_tui.sh

set -eo pipefail

TASK_DESCRIPTION="$1"

if [ -z "$TASK_DESCRIPTION" ]; then
  echo "No description is given. Exitting..."
  exit 1
fi

if ! tmux has-session -t "task"; then
  tmuxp load "task" -d
  sleep 0.1
fi

tmux send-keys -t task:taskwarrior "/description:$TASK_DESCRIPTION" C-m

tmux switch-client -t task
