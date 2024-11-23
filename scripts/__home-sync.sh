#!/bin/env bash

# Help Menu

function help() {
  echo "Usage: $0 [command] [command]"
  echo "Available commands:"
  echo ""
}

# Backup Folder

function rsync_home() {
  rsync -av --delete --files-from=$SCRIPTS/rsync/rsync-home.txt "$HOME/" "$HOME/home/";
  rsync -av --delete --files-from=$SCRIPTS/rsync/rsync-config.txt "$HOME/.config/" "$HOME/home/.config/";
  rsync -av --delete --files-from=$SCRIPTS/rsync/rsync-local.txt "$HOME/.local/share/" "$HOME/home/.local/share/";
  rsync -av --delete "$HOME/.gnupg/" "$HOME/home/.gnupg/";
  rsync -av --delete "$HOME/.icons/" "$HOME/home/.icons/";
  rsync -av --delete "$HOME/.task/" "$HOME/home/.task/";
  rsync -av --delete --exclude=.cache/ --exclude=cache/ "$HOME/.var/" "$HOME/home/.var/";
  rsync -av --delete "$HOME/.zotero/" "$HOME/home/.zotero/";
}

case "$1" in
  "help")
    help
    ;;
  "")
    rsync_home
    ;;
esac
