.POSIX:
CFLAGS=-g -Werror=all -Wextra

dir: dir.o fntable.o
dir.o: dir.c dir.h
fntable.o: fntable.c fntable.h
clean:
	$(RM) dir dir.o fntable.o
