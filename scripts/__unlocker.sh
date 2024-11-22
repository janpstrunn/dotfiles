#!/bin/env bash

# Help Menu

function help() {
  echo "Usage: $0"
  echo "Available commands:"
  echo "help                   - Prints the help menu and exits"
}

# Tmux Integration

function cd-tmux() {
  local SESSION_NAME=$(basename "/mnt/" | tr ' .:' '_')
  tmux new-session -d -s "$SESSION_NAME" -c /mnt/
  tmux attach -t "$SESSION_NAME"
}

# Core Function

function unlock() {
  export vault=$(lsd $HOME/Vault/ | fzf --height 40% --prompt "Select vault to unlock: ")  
  gocryptfs $HOME/Vault/"$vault" /mnt/ &&
  cd-tmux &&
  fusermount3 -u /mnt/
  exit 1
}

case "$1" in
  "help")
    help
    ;;
  "")
    unlock
    ;;
esac
