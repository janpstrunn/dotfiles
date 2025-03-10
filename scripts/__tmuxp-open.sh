#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__tmuxp-open.sh

if [ -z "$TERMCMD" ]; then
  var=$(grep "TERMCMD" "$HOME/.localenv")
  export $var
fi

function tp() {
  session=$(find "$HOME/.config/tmuxp/" -type f -name '*.yaml' -printf '%P\n' | awk -F. '{print $1}')
  select=$(echo "$session" | rofi -dmenu)
  if [[ -n "$select" ]]; then
    if tmux has-session -t "$select"; then
      $TERMCMD -e tmux attach-session -t "$select"
    else
      tmuxp load "$select" -d
      $TERMCMD -e tmux attach-session -t "$select"
    fi
  fi
}

tp
