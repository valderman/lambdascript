#!/bin/bash

# Make sure we're running the latest build
echo
echo "Compiling..."
if make > /dev/null 2> /dev/null ; then
    true
else
    echo "Compiling lsc failed!"
    exit -1
fi
echo

let goodpassed=0
let goodtotal=0
echo "Running good tests..."
for f in tests/should-work/*; do
    let goodtotal=$goodtotal+1
    if ./lsc "$f" > /dev/null 2> /dev/null; then
        let goodpassed=$goodpassed+1
    else
        echo "FAILED: $f"
    fi
done
echo

let failpassed=0
let failtotal=0
echo "Running bad tests..."
for f in tests/should-fail/*; do
    let failtotal=$failtotal+1
    if ./lsc "$f" > /dev/null 2> /dev/null ; then
        echo "FAILED: $f"
    else
        let failpassed=$failpassed+1
    fi
done

echo
echo "$goodpassed/$goodtotal good tests OK"
echo "$failpassed/$failtotal bad tests OK"
