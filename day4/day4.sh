#!/usr/bin/env bash

valid_dups=0; valid_ana=0
while IFS='' read -r line || [[ -n "$line" ]]; do
    br1=0; br2=0
    sorted=""
    for word in $line; do
        # Puzzle 1
        !(( br1 )) && br1=$(grep -ow "$word" <<< "$line" | wc -l) && (( br1-- ))

        # Puzzle 2
        sort_word=$(echo "$word" | grep -o . | sort | tr -d "\n")
        !(( br2 )) && br2=$(grep -ow "$sort_word" <<< "$sorted" | wc -l)

        (( br1 )) && (( br2 )) && break
        sorted+="$sort_word "
    done
    !(( br1 )) && (( valid_dups++ ))
    !(( br2 )) && (( valid_ana++ ))
done < "$1"

echo "Puzzle 1: $valid_dups passphrases are valid!"
echo "Puzzle 2: $valid_ana passphrases are valid!"
