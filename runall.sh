#!/bin/bash

for i in $(seq 1 $1); do
    EXP=$(sed "${i}q;d" expected.txt)
    RES=$(./run.sh "$i")

    if [ "$RES" == "$EXP" ]; then
        echo "Test $i: PASS"
    else
        echo "Test $i: FAIL (Expected: $EXP, Got: $RES)"
    fi
done
