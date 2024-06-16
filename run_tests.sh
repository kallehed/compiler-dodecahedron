#!/bin/bash

LINES=7

for file in tests/ok/*
do
    OUTPUT=$(cargo r -- $file 2> /dev/null)

    if [ $? -ne 0 ]; then
        echo "||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        echo "TEST FAILED FOR $file"
        echo "last $LINES lines of stdout:"
        echo "$OUTPUT" | tail -n $LINES
        echo "||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
    else
        echo "CORRECT: $file"
    fi
done

for file in tests/fail/*
do
    OUTPUT=$(cargo r -- $file 2> /dev/null)

    if [ $? -eq 0 ]; then
        echo "||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
        echo "TEST FAILED FOR (should have failed to compile) $file"
        echo "last $LINES lines of stdout:"
        echo "$OUTPUT" | tail -n $LINES
        echo "||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
    else
        echo "CORRECT: $file"
    fi
done

echo done
