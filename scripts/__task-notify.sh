#!/usr/bin/env bash

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__task-notify.sh

TMP_FILE="/tmp/taskwarrior_notified"
touch "$TMP_FILE"

notify() {
  local ID DESC DUE_TIME DUE_TIME_EPOCH CURRENT_TIME SLEEP_TIME LAST_NOTIFIED FORMATTED_TIME TYPE
  ID=$(jq -r '.id' <<<"$TASK")
  DESC=$(jq -r '.description' <<<"$TASK")
  DUE_TIME=$(jq -r '.due' <<<"$TASK")

  [[ -z "$DUE_TIME" || "$DUE_TIME" == "null" ]] && return

  FORMATTED_TIME=$(echo "$DUE_TIME" | sed -E 's/(.{4})(.{2})(.{2})T(.{2})(.{2})(.{2})Z/\4:\5/')
  DUE_TIME_EPOCH=$(echo "$DUE_TIME" | sed -E 's/(.{4})(.{2})(.{2})T(.{2})(.{2})(.{2})Z/\1-\2-\3 \4:\5:\6/' | xargs -I{} date -u -d "{}" +%s)

  CURRENT_TIME=$(date +%s)
  SLEEP_TIME=$((DUE_TIME_EPOCH - CURRENT_TIME))

  if ((SLEEP_TIME <= 0)); then
    TYPE="overdue"
  else
    TYPE="due"
  fi

  LAST_NOTIFIED=$(awk -v id="$ID" '$1 == id {print $2}' "$TMP_FILE")
  [[ -n "$LAST_NOTIFIED" && $((CURRENT_TIME - LAST_NOTIFIED)) -lt 3600 ]] && return

  sed -i "/^$ID /d" "$TMP_FILE"
  echo "$ID $CURRENT_TIME" >>"$TMP_FILE"

  notify-send -a "taskwarrior" "Task is $TYPE" "$DESC at $FORMATTED_TIME"
}

while true; do
  task status:pending +READY due.before:tomorrow sort:due export | jq -c '.[]' | while read -r TASK; do
    notify
  done

  sleep 60
done
