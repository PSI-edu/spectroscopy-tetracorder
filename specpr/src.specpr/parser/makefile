# @(#) makefile 2.9@(#)

COBJ=	io.o	math.o	history.o	parser.o
#OBJ=	f23.o	${COBJ}
SHELL=/bin/sh

OBJ= f23temp.o

$(SP_OBJ)/parser.o:	$(OBJ)   ../common/spblockdata.o
	$(LD) $(SP_LFLAGS) -r -o $@ f23temp.o
#
# f23temp is a temporary hack to allow for compile.
#
#
#$(SP_OBJ)/parser.o:	${OBJ}
#	$(LD) ${LFLAGS} -r -o $@ ${OBJ} 

f23temp.o:	f23temp.f
	$(F77) $(SP_FFLAGS) -c f23temp.f

f23.o:		f23.r
	$(RF) $(SP_RFLAGS) f23.r > f23.f
	$(F77) $(SP_FFLAGS) -c f23.f
	rm -f f23.f

io.o:		io.c
	$(CC) $(SP_CFLAGS) -c io.c

math.o:		math.c
	$(CC) $(SP_CFLAGS) -c math.c

history.o:	history.c
	$(CC) $(SP_CFLAGS) -c history.c

parser.o:	parser.c y.tab.c lex.yy.c
	$(CC) $(SP_CFLAGS) -c parser.c

lex.yy.c:	lex.l
	$(LEX) $(SP_LFLAGS) $(SP_TMP)/lex.l
	cp lex.yy.c lex.yy.c.save

y.tab.c:	gram.y
	$(YACC) $(YFLAGS) gram.y
	cp y.tab.c y.tab.c.save

