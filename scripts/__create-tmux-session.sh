#!/bin/env bash

# Originally created by Piotr1215 (https://github.com/Piotr1215)

function create_tmux_session() {
  local RESULT="$1"
  local SESSION_NAME=$(basename "$RESULT" | tr ' .:' '_') 
  tmux new-session -d -s "$SESSION_NAME" -c "$RESULT"
  tmux attach -t "$SESSION_NAME"
}

create_tmux_session
