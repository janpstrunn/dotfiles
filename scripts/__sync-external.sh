#!/bin/env bash

## External Drive Path

filen="$HOME/Leviathan/Filen/"
ente="$HOME/Leviathan/Ente/"
mega="$HOME/Leviathan/MEGA/"

leviathan="$HOME/Leviathan/"
pandora="$HOME/Pandora/"

beelzebub="/run/media/janpstrunn/B3LZ33BUB/"
seth="/run/media/janpstrunn/S3TH"

# Syncing External Drives

function syncbeelzebub() {
  rsync -av --delete $pandora $beelzebub/Pandora/;
  rsync -av --delete $filen $beelzebub/Filen/;
  rsync -av --delete $ente $beelzebub/Ente/;
}

function syncseth() {
  rsync -av --delete $pandora $seth/Pandora/;
  rsync -av --delete $filen $seth/Filen/;
  rsync -av --delete $ente $seth/Ente/;
}

if [[ "$1" == "1" ]]; then
  syncbeelzebub
elif [[ "$1" == "2" ]]; then
  syncseth
else
  echo "Choose drive to sync:"
  echo "1 - Beelzebub"
  echo "2 - Seth"
  exit 1
fi
