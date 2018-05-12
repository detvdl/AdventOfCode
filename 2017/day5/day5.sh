#!/usr/bin/env bash

IFS=$'\n' read -d '' -r -a array < "$1"
arrcpy=("${array[@]}")

# Puzzle 1
steps=0
pos=0
while [ "$pos" -lt "${#array[@]}" ]; do
    (( steps++ ))
    offset=${array[pos]}
    (( array[pos]++ ))
    (( pos += offset ))
      done
echo "Puzzle 1: the amount of steps required to exit is: $steps"

# Puzzle 2
steps=0
pos=0
while [ "$pos" -ge "0" ] && [ "$pos" -lt "${#arrcpy[@]}" ]; do
    (( steps++ ))
    offset=${arrcpy[pos]}
    [ "${arrcpy[pos]}" -ge "3" ] && (( arrcpy[pos]-- )) || (( arrcpy[pos]++ ))
    (( pos += offset ))
done
echo "Puzzle 2: the amount of steps required to exit is: $steps"
