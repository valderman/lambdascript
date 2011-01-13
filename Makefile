jlc:
	make -C src

test:
	./runtests.sh

failed:
	./runtests.sh --failed

doc:
	make -C src doc

clean:
	make -C src clean
	rm failed-bad failed-good
