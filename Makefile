all: openemacs

openemacs: openemacs.c
# Add -fsanitize=address to compile with ASAN. Incompatible with valgrind.
	$(CC) -o openemacs openemacs.c -g -O0 -Wall -Wextra -Werror -W -pedantic -std=c11 -Wformat=2 -Wswitch-default -Wunused

indent:
	astyle -n --indent=spaces=4 --style=attach --max-code-length=160 --lineend=linux --delete-empty-lines --convert-tabs --align-pointer=name --add-brackets --keep-one-line-blocks openemacs.c

lint:
	cppcheck --enable=all --inconclusive openemacs.c

clean:
	rm -f openemacs
