PROGRAM=03_filtered_sum.out
CXX       = g++
CXXFLAGS  = -g -std=c++14 -Wall -I../range-v3-0.11.0/include

$(PROGRAM): main.o
	$(CXX) -o $(PROGRAM) main.o

.PHONY: clean dist

clean:
	-rm *.o $(PROGRAM) *core

dist: clean
	-tar -chvj -C .. -f ../$(PROGRAM).tar.bz2 $(PROGRAM)


