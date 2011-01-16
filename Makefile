jlc:
	make -C src

test:
	./runtests.sh

failed:
	./runtests.sh --failed

example:
	make -C examples

doc:
	make -C src doc

clean:
	make -C src clean
	make -C examples clean
	rm failed-bad failed-good
