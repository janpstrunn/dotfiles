#!/bin/bash

export pass=$(ls $VAULT | grep ".asc" | fzf --height 40% --prompt "Select pgp file to output: ")

gpg --decrypt $VAULT/$pass | xclip -sel clip

sleep 5

echo "" | xclip -sel clip
