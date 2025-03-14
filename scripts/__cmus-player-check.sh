#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__cmus-player-check.sh

function main() {
  current_state=$(cmus-remote -Q | grep "status" | awk '{print $2}')
  if [ "$current_state" = "playing" ]; then
    cmus-remote -u
  elif [ "$current_state" = "paused" ]; then
    cmus-remote -p
  else
    echo "Cmus isn't playing or paused"
    notify-send -u normal "Cmus isn't playing or paused"
  fi
}

main
