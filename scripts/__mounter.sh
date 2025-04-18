#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__mounter.sh

function help() {
  echo "External Device Mounter"
  echo "Usage: $0 [option]"
  echo "Available options:"
  echo "add                           - Creates a directory in /mnt/"
  echo "open                          - Open luks device and mount it using pass"
  echo "iopen                         - Open luks device and mount it using prompted password"
  echo "close                         - Closes luks device and umount it"
}

function adddrive() {
  echo "Existing drives:"
  ls /mnt/
  read -p "Name your drive: " drivename
  if [ ! -d "/mnt/$drivename" ]; then
    echo "No directory found at /mnt/$drivename. Creating one now..."
    sudo mkdir "/mnt/$drivename"
    sudo chown "$USER:$USER" "/mnt/$drivename"
    echo "Directory /mnt/$drivename created."
    sleep 1
    exit 0
  elif [ -d "mnt/$drivename" ]; then
    echo "Drive already exists!"
    sleep 1
    exit 1
  fi
}

function _mount() {
  device=$(ls /dev | grep -E "sd[a-z]+[0-9]+$" | fzf --prompt "Choose a drive to mount: ")
  drive=$(ls /mnt/ | fzf --prompt "Previously mounted devices: ")
  passdir="${PASSWORD_STORE_DIR:-$HOME/.password-store}"
  pass="$(ls "$passdir/dev/" | awk -F. '{print $1}' | grep "$drive")"
  if [ "$pass" == "$drive" ]; then
    password=$(pass "dev/$pass")
  else
    echo "$drive isn't in your pass store!"
    sleep 1
    exit 1
  fi
  echo "$password" | sudo cryptsetup luksOpen "/dev/$device" "$drive" && echo "$device has been opened and named as $drive!"
  sudo mount "/dev/mapper/$drive" "/mnt/$drive/" && echo "$drive has been mounted to /mnt/$drive!"
  echo "" | xclip -sel clip
  sleep 1
  exit 0
}

function _imount() {
  device=$(ls /dev | grep -E "sd[a-z]+[0-9]+$" | fzf --prompt "Choose a drive to mount: ")
  echo "Previously mounted devices:" && ls /mnt/
  read -p "Name the drive: " drive
  sudo cryptsetup luksOpen "/dev/$device" "$drive" && echo "$device has been opened and named as $drive!"
  sudo mount "/dev/mapper/$drive" "/mnt/$drive/" && echo "$drive has been mounted to /mnt/$drive!"
  sleep 1
  exit 0
}

function _umount() {
  drive=$(ls /dev/mapper/ | fzf --prompt "Choose a drive to umount: ")
  sudo umount "/mnt/$drive/"
  sudo cryptsetup luksClose "$drive" && echo "$drive has been umounted!"
  sleep 1
  exit 0
}

function read_option() {
  while true; do
    help
    read -p "Choose an option (add/iopen/open/close): " option
    if [ "$option" == "open" ]; then
      _mount
    elif [ "$option" == "iopen" ]; then
      _imount
    elif [ "$option" == "close" ]; then
      _umount
    elif [ "$option" == "add" ]; then
      adddrive
    else
      exit 0
    fi
  done
}

case $1 in
"help")
  help
  ;;
"open")
  _mount
  ;;
"iopen")
  _imount
  ;;
"close")
  _umount
  ;;
*)
  read_option
  ;;
esac
