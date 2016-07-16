all: kilo

kilo: kilo.c
	$(CC) -o kilo kilo.c -Wall -Wextra -Werror -W -pedantic -std=c99 -Os
	strip kilo

clean:
	rm -f kilo
