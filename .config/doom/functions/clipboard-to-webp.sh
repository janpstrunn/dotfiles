#!/usr/bin/env bash

trap cleanup SIGINT SIGTERM

cleanup() {
  echo "An error occured!"
  rm "$TMP_FILE"
  exit 1
}

ORG_DIR="$HOME/org/attach"

if [ ! -d "$ORG_DIR" ]; then
  mkdir -p "$ORG_DIR"
fi

TIMESTAMP=$(date +"%Y%m%d")
IMAGENAME=$(zenity --entry --title="Enter image name" --text="Please enter the image name:" --hide-text 2>/dev/null)
CLIPMETHOD="$XDG_SESSION_TYPE"
FILENAME="$IMAGENAME-$TIMESTAMP.webp"
TMP_FILE="$IMAGENAME-tmp.png"
ORG_PATH="$ORG_DIR/$FILENAME"

if [ "$CLIPMETHOD" = "x11" ]; then
  xclip -selection clipboard -t image/png -o >"$TMP_FILE"
elif [ "$CLIPMETHOD" = "wayland" ]; then
  wl-paste --type image/png >"$TMP_FILE"
fi

magick "$TMP_FILE" -quality 75 "$ORG_PATH"

echo "$ORG_PATH"
