#!/bin/env bash

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

case "$1" in
"edit") session=edit ;;
"master") session=master ;;
esac

tmux_profile
