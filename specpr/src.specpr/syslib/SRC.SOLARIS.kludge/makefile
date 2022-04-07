CFLAGS= -g
FFLAGS= -g -C
LFLAGS=
RFLAGS= -6% -C
MCHINE=
DBG=../debug
GET= get
GFLAGS=

OBJ=	\
../bkrec.o \
../frdrec.o	\
../wrttap.o	\
../redtap.o

../../syslib.o:	$(OBJ)	



../bkrec.o:
	$(CC) $(CFLAGS) -c -o ../bkrec.o bkrec.c

../frdrec.o:
	$(CC) $(CFLAGS) -c -o ../frdrec.o frdrec.c

../wrttap.o:
	$(CC) $(CFLAGS) -c -o ../wrttap.o wrttap.c

../redtap.o:
	$(CC) $(CFLAGS) -c -o ../redtap.o redtap.c

#../bkrec.o ../frdrec.o:	../tape.h
