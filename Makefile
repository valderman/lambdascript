jlc:
	make -C src

test:
	@echo "Running tests with TCE..."
	@./runtests.sh
	@echo "Running tests without TCE, expect any TCE-dependent test cases to fail!"
	@LSCFLAGS=--no-tce ./runtests.sh

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
