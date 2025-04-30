#!/usr/bin/env bash

restart="Alt-r"

function get_urls() {
  lynx -dump "https://leta.mullvad.net/search?q=$query&engine=brave" | rg --no-line-number '  https://' | tr -d " "
}

function _rofi() {
  rofi -dmenu -i -no-levenshtein-sort -width 1000 -kb-custom-1 "${restart}" "$@"
}

function get_input() {
  search=$(echo "" | _rofi -p "> " -mesg "Enter your Search")
  [[ -z $search ]] && exit 1
  query=$(echo "$search" | sed 's/ /+/')
}

function main() {
  get_input
  select=$(get_urls | _rofi -p "󰖟 ")
  val=$?
  case "$val" in
  10)
    main
    ;;
  esac
  xdg-open "$select" >/dev/null
}

main
