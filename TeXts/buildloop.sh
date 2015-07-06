#!/bin/bash

function build_it {
  make 2>&1  \
        | ack --flush --passthru --color --color-match=red    "^.*FAIL.*" \
        | ack --flush --passthru --color --color-match=red    "^.*Fail.*" \
        | ack --flush --passthru --color --color-match=red    "^.*fail.*" \
        | ack --flush --passthru --color --color-match=yellow "^.*Warning.*" \
        | ack --flush --passthru --color --color-match=green  "^.*PASS*"

  if [ ${PIPESTATUS[0]} -eq 0 ]
  then
    echo
    echo -e "\tBuild OK" | ack --flush --passthru --color --color-match=green "."
  else
    echo
    echo -e "\tBuild FAILED" | ack --flush --passthru --color --color-match=red "."
    #beep
  fi

  echo
  echo
}

make clean
build_it

while true; do
  inotifywait -r -e modify .

  build_it
done
