#!/bin/bash

# abook --outformat doesn't support custom fields :/
# XXX: doesn't work for updates, creates a new event always
# abook-to-birthdays.sh | sh

<~/.abook/addressbook gawk '
/^\[[0-9]/ { gsub(/[^0-9]/,"",$0); idx=$0 }
/^birthday=/ {
  n = split($0, x, /[-=]/)
  day = x[n]
  month = x[n-1]
  year = x[n-2]
  printf "khal new -a birthdays -r yearly -m 7d,1d,0d %d.%d. \"%s\" :: \"%s\" %d.%d.%d\n", day, month, name, name, day, month, year
}
/^name=/ {
  gsub(/name=/, "")
  name = $0
}
'
