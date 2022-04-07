GET= get
GFLAGS=-s

OBJ=sspp.o 

sspp:	$(OBJ)
	cc $(CFLAGS) -o sspp $(OBJ)

install:	
	mv -f sspp /usr/local/bin
	chmod 555 /usr/local/bin/sspp

manual:		manual.1
	cp manual.1 /usr/man/cat1/sspp.1

.SUFFIXES:
.SUFFIXES: .o .c
	
sspp.o:		sspp.c
	cc $(CFLAGS) -c sspp.c
