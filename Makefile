jlc:
	make -C src

test:
	./runtests.sh

failed:
	./runtests.sh --failed

example:
	make -C examples/scroller

doc:
	make -C src doc

clean:
	make -C src clean
	rm failed-bad failed-good
