#!/usr/bin/env bash

function duplicates {
    dups=$(echo "$1" | tr "[:blank:]" " " | tr "[:blank:]" "\n" | sort | uniq -d)
    [ -z "$dups" ] && return 1 || return 0
}

function anagrams {
    sorted=""
    for word in $1; do
        sorted+=$(echo "$word" | grep -o . | sort | tr -d "\n")
        sorted+=" "
    done
    return $(duplicates "$sorted")
}

valid_dups=0
valid_ana=0
while IFS='' read -r line || [[ -n "$line" ]]; do
    # Puzzle 1
    ! duplicates "$line" && ((valid_dups++))
    # Puzzle 2
    ! anagrams "$line" && ((valid_ana++))
done < "$1"

echo "Puzzle 1: $valid_dups passphrases are valid!"
echo "Puzzle 2: $valid_ana passphrases are valid!"
