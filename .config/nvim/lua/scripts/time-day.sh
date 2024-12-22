#!/bin/env bash

Year=$(date +%Y)
Month=$(date +%B)
Day=$(date +%d)

if [ $Day -eq 1 ]; then
  OrdinalSuffix="st"
elif [ $Day -eq 2 ]; then
  OrdinalSuffix="nd"
elif [ $Day -eq 3 ]; then
  OrdinalSuffix="rd"
elif [ ${#Day} -gt 1 ] && [ ${Day: -2} -eq 11 ]; then
  OrdinalSuffix="th"
elif [ ${#Day} -gt 1 ] && [ ${Day: -2} -eq 12 ]; then
  OrdinalSuffix="th"
elif [ ${#Day} -gt 1 ] && [ ${Day: -2} -eq 13 ]; then
  OrdinalSuffix="th"
elif [ ${#Day} -gt 1 ] && [ ${Day: -2} -eq 21 ]; then
  OrdinalSuffix="st"
elif [ ${#Day} -gt 1 ] && [ ${Day: -2} -eq 22 ]; then
  OrdinalSuffix="nd"
elif [ ${#Day} -gt 1 ] && [ ${Day: -2} -eq 23 ]; then
  OrdinalSuffix="rd"
elif [ ${#Day} -gt 1 ] && [ ${Day: -2} -eq 31 ]; then
  OrdinalSuffix="st"
else
  OrdinalSuffix="th"
fi

daily=$(echo "${Month} ${Day}${OrdinalSuffix}, ${Year}")

echo "$daily"
