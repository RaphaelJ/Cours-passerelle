CC=gcc -std=c99 -Wall -O2
# CC=clang -std=c99 -Wall -O2

CODAC=../dist/build/coda/coda
LLVMC=llc -O2 -filetype=obj

all:	quicksort game_of_life

quicksort:	quicksort.c quicksort.o
	$(CC) quicksort.c quicksort.o -o quicksort
	
quicksort.o:	quicksort.coda
	$(CODAC) < quicksort.coda | $(LLVMC) > quicksort.o

game_of_life:	game_of_life.c game_of_life.o
	$(CC) game_of_life.c game_of_life.o -o game_of_life

game_of_life.o:	game_of_life.coda
	$(CODAC) < game_of_life.coda | $(LLVMC) > game_of_life.o

clean:
	rm -fv *.o quicksort game_of_life
