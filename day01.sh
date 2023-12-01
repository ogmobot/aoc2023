#!/usr/bin/env bash

digitsfile=$(mktemp)

function day01solve {
    cut -c 1 $digitsfile | paste -d "" - <(grep -o ".$" $digitsfile) \
        | paste -s -d "+" | grep "." | bc
}

# part 1
cat input01.txt | tr -d "[:alpha:]" > $digitsfile
day01solve

# part 2
cat input01.txt \
    | sed "s/one/o1e/g" \
    | sed "s/two/t2o/g" \
    | sed "s/three/t3e/g" \
    | sed "s/four/f4r/g" \
    | sed "s/five/f5e/g" \
    | sed "s/six/s6x/g" \
    | sed "s/seven/s7n/g" \
    | sed "s/eight/e8t/g" \
    | sed "s/nine/n9e/g" \
    | tr -d "[:alpha:]" > $digitsfile
day01solve

rm $digitsfile
