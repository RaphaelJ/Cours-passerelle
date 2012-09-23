EXEC = main

# CC = clang
CC = gcc
CFLAGS  = --std=c99 --pedantic -Wall -W -Wmissing-prototypes -O2
LDFLAGS = 

SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)

$(EXEC): $(OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

clean:
	rm -fv *.o $(EXEC) *~