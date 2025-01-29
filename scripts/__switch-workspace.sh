#!/bin/env bash

if [ -z "$OBSIDIAN" ]; then
  source "$HOME/.env"
fi

function switch_workspace() {
  choose=$(ls $OBSIDIAN | rofi -dmenu)
  if [ "$choose" = "" ]; then
    notify-send -u low "No vault has been selected"
    exit
  fi
  echo "$OBSIDIAN/$choose" > "$HOME/.config/obsidian-workspace.conf"
}

switch_workspace
