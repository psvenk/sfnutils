.POSIX:
CFLAGS=-g -Werror=all -Werror=switch -Wextra

ls8.3: ls8.3.o fntable.o
ls8.3.o: ls8.3.c ls8.3.h
fntable.o: fntable.c fntable.h
clean:
	$(RM) ls8.3 ls8.3.o fntable.o
