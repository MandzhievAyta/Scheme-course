#!/bin/bash
while true
do
  read -r graph <&3 || break
  read -r max_amount <&3 || break
  TAGGED_GRAPH=$(echo -e "$graph\n$max_amount" | racket genetic.rkt)
#echo $TAGGED_GRAPH
  read -r expected_res <&4 || break
  CHECKER_INPUT=""

  if [ "$expected_res" == "#t" ]; then
    read -r expected_amount <&4 || break
    CHECKER_INPUT=$(printf '%s\n%s\n%s\n' "$expected_res" "$expected_amount" "$TAGGED_GRAPH")
  else
    CHECKER_INPUT=$(printf '%s\n%s\n' "$expected_res" "$TAGGED_GRAPH")
  fi
  echo "$CHECKER_INPUT" | racket checker.rkt
done 3<input_file.txt 4<result_file.txt
