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



../bkrec.o:	bkrec.c
	$(CC) $(SP_CFLAGS) -c -o ../bkrec.o bkrec.c

../frdrec.o:	frdrec.c
	$(CC) $(SP_CFLAGS) -c -o ../frdrec.o frdrec.c

../wrttap.o:	wrttap.c
	$(CC) $(SP_CFLAGS) -c -o ../wrttap.o wrttap.c

../redtap.o:	redtap.c
	$(CC) $(SP_CFLAGS) -c -o ../redtap.o redtap.c

#../bkrec.o ../frdrec.o:	../tape.h
