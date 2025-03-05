#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/download-freetube.sh

file=$1   # FreeTube.db
format=$2 # video/audio

if [[ -z "$file" || -z "$format" ]]; then
  echo "Usage: $0 [file] [format]"
  echo "Format: video/audio"
  exit 1
fi

function get_url() {
  playlist=$(jq -r '.playlistName' $file | fzf --prompt "Select a playlist: ")
  if [[ -z "$playlist" ]]; then
    echo "No playlist selected. Exiting."
    exit 1
  fi
  jq -r "select(.playlistName == \"$playlist\") | .videos[].videoId | \"https://youtu.be/\" + ." "$file" >$PWD/"$playlist".txt
  if [[ ! -s "$playlist.txt" ]]; then
    echo "No videos found in the selected playlist. Exiting."
    exit 1
  fi
}

function download() {
  ~/scripts/__download-youtube.sh -b $PWD/"$playlist".txt $format
}

get_url
download
