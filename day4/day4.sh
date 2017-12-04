#!/usr/bin/env bash

method1() {
    valid_dups=0; valid_ana=0
    while IFS='' read -r line || [[ -n "$line" ]]; do
        sorted=""
        # Puzzle 1
        trline=$(echo "$line" | tr "[:blank:]" " " | tr " " "\n")
        [ $(sort -u <<< "$trline" | wc -l) -eq $(wc -l <<< "$trline") ] && (( valid_dups++ ))

        for word in $line; do
            # Puzzle 2
            sort_word=$(echo "$word" | grep -o . | sort | tr -d "\n")
            sorted+="$sort_word "
        done
        trline=$(echo "$sorted" | tr "[:blank:]" " " | tr " " "\n")
        [ $(sort -u <<< "$trline" | wc -l) -eq $(wc -l <<< "$trline") ] && (( valid_ana++ ))
    done < "$1"

    echo "Puzzle 1: $valid_dups passphrases are valid!"
    echo "Puzzle 2: $valid_ana passphrases are valid!"
}

method2() {
    valid_dups=0; valid_ana=0
    while IFS='' read -r line || [[ -n "$line" ]]; do
        br1=1; br2=1
        sorted=(); seen=()
        for word in $line; do
            IFS='|'
            # Puzzle 1
            [[ "$word" =~ $(echo ^\("${seen[*]// /|}"\)$) ]] && br1=0 || seen+=("$word")

            # Puzzle 2
            if [[ (( br2 )) && (( br1 )) ]]; then
                sort_word=$(echo "$word" | grep -o . | sort | tr -d "\n")
                [[ "$sort_word" =~ $(echo ^\("${sorted[*]// /|}"\)$) ]] \
                    &&  br2=0 || sorted+=("$sort_word")
            fi
            IFS=$' \t\n'
            !(( br1 )) && br2=$br1 && break
        done
        (( valid_dups+=br1 ))
        (( valid_ana+=br2 ))
    done < "$1"
    echo "Puzzle 1: $valid_dups passphrases are valid!"
    echo "Puzzle 2: $valid_ana passphrases are valid!"
}

if [ "$#" -ne 1 ]; then
    echo "ERROR: Please provide the input filename as parameter..."
    exit
fi

method2 "$1"
