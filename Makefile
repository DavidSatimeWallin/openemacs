all: kilo

kilo: kilo.c
	$(CC) -o kilo kilo.c -Wall -Wextra -Werror -W -pedantic -std=c11 -fsanitize=address -Wformat=2 -Wswitch-default -Wunused -Os
# -Weverything
	strip kilo

indent:
	astyle -n --indent=spaces=4 --style=attach --max-code-length=160 --lineend=linux --delete-empty-lines --convert-tabs --align-pointer=name --add-brackets --keep-one-line-blocks kilo.c

clean:
	rm -f kilo
