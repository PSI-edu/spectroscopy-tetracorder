SHELL=/bin/sh
SRCFIL=../..

OBJ=		\
sp_stamp.o

sp_stamp:	$(OBJ)
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o sp_stamp $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


sp_stamp.o:		sp_stamp.r
	$(SSPP) -r -$(SSPPFLAGS)  sp_stamp.r > sp_stamp.x
	$(RF) $(RFLAGS) sp_stamp.x > sp_stamp.f
	$(F77) $(FFLAGS) -c sp_stamp.f
	rm -f sp_stamp.x sp_stamp.f

install:	
	mv -f sp_stamp $(SP_LOCAL)/bin/sp_stamp
	chmod 755 $(SP_LOCAL)/bin/sp_stamp
