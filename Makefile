jlc:
	make -C src

test:
	./runtests.sh

failed:
	./runtests.sh --failed

clean:
	make -C src clean
	rm failed-bad failed-good
