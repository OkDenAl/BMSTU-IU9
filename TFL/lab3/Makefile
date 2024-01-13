.PHONY: generate build-project run test valgrind-run run-fuzz
.SILENT: run

generate:
	mkdir build
	make build-project

build-project:
	cd ./build && cmake .. && make --no-print-directory

run:
	./build/lab3 -a abc -ra 10 -mtc 10000 -pi 100

valgrind-run:
	valgrind --tool=memcheck make run

check:
	cppcheck --language=c++ ./project/src/*.cpp ./main.cpp
