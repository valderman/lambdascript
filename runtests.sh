#!/bin/bash

recompile() {
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
}

test_lib() {
    echo "Building all libs for sanity check..."
    for f in $(ls lib | egrep '\.ls$') ; do
      ./lsc "$f" > /dev/null 2> /dev/null
    done
}

test_all() {
    recompile

    test_lib

    # Remove results of previous test runs
    echo "" | tee failed-good failed-bad

    # Run good tests
    let goodpassed=0
    let goodtotal=0
    echo "Running good tests..."
    for f in tests/should-work/*; do
	let goodtotal=$goodtotal+1
	if ./lsc -mmain "$f" > /dev/null 2> /dev/null ; then
	    # blah.ls -> blah.js
	    jsfile=a.out.js
	    echo "print(main.main());" >> $jsfile

        oracle=tests/oracles/$(basename $(echo $f | sed -e 's/\(\\*\).ls$/\1.js/'))
	    if [ -e $oracle ] ; then
		oracleres=$(cat $oracle)
		result=$(js $jsfile 2> /dev/null)
		if [ "$oracleres" == "$result" ] ; then
		    let goodpassed=$goodpassed+1
		else
		    echo "Wrong result: $f"
		    echo "  (expected '$oracleres', got '$result')"
		    echo "$f" >> failed-good
		fi
	    else
		echo "No oracle for $f"
		echo "$f" >> failed-good
	    fi
	else
            echo "Failed to compile: $f"
	    echo "$f" >> failed-good
	fi
    done
    echo

    # Run bad tests
    let failpassed=0
    let failtotal=0
    echo "Running bad tests..."
    for f in tests/should-fail/*; do
	let failtotal=$failtotal+1
	if ./lsc "$f" > /dev/null 2> /dev/null ; then
            echo "FAILED: $f"
	    echo "$f" >> failed-bad
	else
            let failpassed=$failpassed+1
	fi
    done
}

test_failed() {
    recompile

    # If there are no records of old test runs, run all tests before going
    # details.
    if [ ! -e failed-good ] || [ ! -e failed-bad ] ; then
	    test_all
    fi

    let goodpassed=0
    let goodtotal=0
    echo "Running good tests..."
    for f in `cat failed-good`; do
	let goodtotal=$goodtotal+1
	if ./lsc -mmain "$f" ; then
            let goodpassed=$goodpassed+1
	else
            echo "FAILED: $f"
	fi
    done
    echo

    let failpassed=0
    let failtotal=0
    echo "Running bad tests..."
    for f in `cat failed-bad`; do
	let failtotal=$failtotal+1
	if ./lsc "$f" ; then
            echo "FAILED: $f"
	else
            let failpassed=$failpassed+1
	fi
    done
}

run="all"
for a in $@ ; do
    if [ "$a" == "--failed" ] ; then
	run="failed"
    fi
done

if [ "$run" == "failed" ] ; then
    test_failed
else
    test_all
fi

echo
echo "$goodpassed/$goodtotal good tests OK"
echo "$failpassed/$failtotal bad tests OK"
