#!/bin/bash
racket generate_tests.rkt |
while read line; do 
  EXPECTED_RES=$(echo $line | awk '{print $(NF-1)}')
  RES=$(echo "$line" | awk '{$NF=""; $(NF-1)=""; print$0}' | racket tag_graph.rkt)
  if [ "$RES" == "$EXPECTED_RES" ]; then
    echo OK
  else
    echo FAIL
  fi
done

