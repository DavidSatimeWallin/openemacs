all: kilo

kilo: kilo.c
	$(CC) -o kilo kilo.c -Wall -Wextra -Werror -W -pedantic -std=c99

clean:
	rm -f kilo
