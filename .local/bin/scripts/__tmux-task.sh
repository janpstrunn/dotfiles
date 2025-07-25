#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__tmux-task.sh

# "Taskwarrior + Nvim integration to show completed tasks
# Support file formats:
# YYYY-MM-DD.md - Day"
# YYYY-MM.md - Month"
# YYYY-[W]WW.md - Week"
# YYYY.md - Year"

ENV_FILE="$HOME/.scriptenv"

if [ -z "$ENV_FILE" ]; then
  echo ".scriptenv is missing at $HOME!"
  return 1
fi

source "$ENV_FILE"

if [ -z "$SHELL" ] || [ -z "$SCRIPTS" ]; then
  echo "Error: SHELL or SCRIPTS env at .scriptenv not found"
  exit 1
fi

if ! command -v task &>/dev/null; then
  echo "task could not be found. Please install it."
  exit 1
fi

current_command=$(tmux display-message -p "#{pane_current_command}")

if [[ "$current_command" == "nvim" ]]; then
  tmux send-keys -t 0 ":call writefile([fnamemodify(expand('%:p'), ':t')], '/tmp/filename_output.txt')" C-m
  sleep 0.1
  file_name=$(cat /tmp/filename_output.txt)
  file_name=$(echo "$file_name" | sed 's/\.md//; s/th//')

  if [[ -n "$file_name" ]]; then
    tmux splitw -v \
      "$SHELL" -c "$SCRIPTS/__tskd.sh \"$file_name\" tmux && exec $SHELL"
  else
    echo "Could not retrieve the file name."
  fi
else
  echo "Not in nvim."
fi
