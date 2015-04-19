#!/bin/bash

cabal clean
cabal configure --enable-tests --disable-optimization --disable-library-coverage
rm -r tests.tix  .hpc/

while true; do
  inotifywait -r -e modify src/ testsuite/ FrozenBeagle.cabal  

  cabal test 2>&1  \
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
    beep
  fi

  echo
  echo
done
