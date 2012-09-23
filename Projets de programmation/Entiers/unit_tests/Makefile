EXEC = UnitTests

HSC = ghc
HSC2HS = hsc2hs
HSFLAGS  = -O2 -Wall

# CC = clang
CC = gcc
CFLAGS  = --std=c99 --pedantic -Wall -W -Wmissing-prototypes -O2
LDFLAGS =

all: UnitTests.hs unit_tests_wrapper.o ../integers.o ../comparaisons.o ../arithmetic.o ../digits.o
	$(HSC) UnitTests.hs $(HSFLAGS) -o $(EXEC) unit_tests_wrapper.o ../integers.o ../comparaisons.o ../arithmetic.o ../digits.o

%.hs: %.hsc
	$(HSC2HS) $<

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

clean:
	rm -fv *.o UnitTests.hs $(EXEC) *~