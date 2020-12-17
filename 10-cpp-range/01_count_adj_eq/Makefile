PROGRAM=01_count_adj_eq.out
CXX       = g++
CXXFLAGS  = -g -std=c++14 -Wall -I../range-v3/include

$(PROGRAM): main.o
	$(CXX) -o $(PROGRAM) main.o

.PHONY: clean dist

clean:
	-rm *.o $(PROGRAM) *core

dist: clean
	-tar -chvj -C .. -f ../$(PROGRAM).tar.bz2 $(PROGRAM)


