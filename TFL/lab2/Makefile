.PHONY: generate build-project run run-all-tests valgrind-run run-fuzz
.SILENT: run

generate:
	mkdir build
	make build-project

build-project:
	cd ./build && cmake .. && make --no-print-directory

# менять значения тут
run:
	./build/simplifier -f test.txt -r ^ab$

run-all-tests:
	./build/test/fuzz_test
	./build/test/parser_test
	./build/test/sgen_test
	./build/test/to_regex_test
	./build/test/tset_test

run-test:
	./build/test/sgen_test

run-fuzz:
	./build/test/fuzz_test

valgrind-run:
	valgrind --tool=memcheck make run

check:
	cppcheck --language=c++ ./project/src/*.cpp ./main.cpp

console: 
	./build/console
