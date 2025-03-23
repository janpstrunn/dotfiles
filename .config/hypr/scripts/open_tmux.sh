#!/usr/bin/env bash

function tpo() {
  local session_name="$1"
  local class_name="tmux-$session_name"

  if tmux has-session -t "$session_name" 2>/dev/null; then
    if pgrep -f "tmux attach-session -t $session_name" >/dev/null; then
      hyprctl dispatch focuswindow class:"$class_name"
    else
      exec kitty --class "$class_name" -e tmux attach-session -t "$session_name"
    fi
  else
    exec kitty --class "$class_name" -e tmuxp load "$session_name" -d && tmux attach-session -t "$session_name"
  fi
}

tpo "$1"
