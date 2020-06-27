ls8.3: ls8.3.o fntable.o
	cc -g -o ls8.3 ls8.3.o fntable.o

ls8.3.o: ls8.3.c ls8.3.h
	cc -c ls8.3.c

fntable.o: fntable.c fntable.h
	cc -c fntable.c

clean:
	rm ls8.3 ls8.3.o fntable.o
