#!/bin/env bash

function startup() {
  if tmux has-session -t "invidious"; then
    echo "Invidious Instance is already running."
  else
    tmuxp load "invidious" -d
    echo "Invidious Instance launched."
  fi
}

function main() {
  startup
  freetube
}

main
