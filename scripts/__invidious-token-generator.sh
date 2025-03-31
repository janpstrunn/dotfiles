#!/usr/bin/env sh

# https://github.com/janpstrunn/dotfiles/blob/main/scripts/__invidious-token-generator.sh

invidious_config=$HOME/invidious/docker-compose.yml

docker run quay.io/invidious/youtube-trusted-session-generator >token.txt

visitor_data=$(grep "visitor_data" token.txt | awk -F ':' '{print $2}')
po_token=$(grep "po_token" token.txt | awk -F ':' '{print $2}')

sed -i "s/^\([[:space:]]*visitor_data:\)[[:space:]]*.*/\1 $visitor_data/" "$invidious_config" && visitor_ok=True
sed -i "s/^\([[:space:]]*po_token:\)[[:space:]]*.*/\1 $po_token/" "$invidious_config" && po_ok=True

if [ "$visitor_ok" == "True" ] || [ "$po_ok" == "True" ]; then
  echo "Tokens successfully updated!"
else
  echo "Tokens failed to be updated!"
fi
