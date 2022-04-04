# Commands:

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o scavenge Main.hs

prof: 
	ghc --make -prof -o scavenge Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f scavenge
	rm -f *.hi
	rm -f *.o

test: build
	./scavenge --test
