#!/usr/bin/env bash

NOTES_DIR="$HOME/org/journal"
DAILY_DIR="$NOTES_DIR/daily"

CURRENT_FILE=$(basename "$1")
YEAR_REGEX="^[0-9]{4}\.org$"
MONTH_REGEX="^[0-9]{4}-[0-9]{2}\.org$"
WEEK_REGEX="^[0-9]{4}-W[0-9]{2}\.org$"
DAY_REGEX="^[0-9]{4}-[0-9]{2}-[0-9]{2}\.org$"

YEAR=${CURRENT_FILE:0:4}
WEEK_NUM=${CURRENT_FILE:6:2}

get_weekly_dates() {
  FIRST_DAY=$(date -d "$YEAR-01-04" +%u)
  WEEK_START=$(date -d "$YEAR-01-04 -$((FIRST_DAY - 1)) days +$(((WEEK_NUM - 1) * 7)) days" +%Y-%m-%d)

  for i in {0..6}; do
    date -d "$WEEK_START +$i days" +%Y-%m-%d
  done
}

find_notes() {
  if [[ $CURRENT_FILE =~ $YEAR_REGEX ]]; then
    find "$DAILY_DIR" -type f -name "$YEAR-*.org"

  elif [[ $CURRENT_FILE =~ $MONTH_REGEX ]]; then
    find "$DAILY_DIR" -type f -name "${CURRENT_FILE%????}-*.org"

  elif [[ $CURRENT_FILE =~ $WEEK_REGEX ]]; then
    for day in $(get_weekly_dates); do
      echo "$DAILY_DIR/$day.org"
    done

  elif [[ $CURRENT_FILE =~ $DAY_REGEX ]]; then
    echo "$DAILY_DIR/$CURRENT_FILE"
  fi
}

FILES=$(find_notes)

if [[ -n "$FILES" ]]; then
  rg "🌟" $FILES
else
  echo "No relevant notes found!"
fi
