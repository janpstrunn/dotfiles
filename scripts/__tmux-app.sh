#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__tmux-app.sh

if [ -z "$TERMCMD" ]; then
  var=$(grep "TERMCMD" "$HOME/.localenv")
  export $var
fi

function tmux_profile() {
  if tmux has-session -t "$session"; then
    $TERMCMD -e tmux attach-session -t "$session"
  else
    tmuxp load "$session" -d
    $TERMCMD -e tmux attach-session -t "$session"
  fi
}

session=$1

tmux_profile
