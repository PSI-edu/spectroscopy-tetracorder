SHELL=/bin/sh

OBJ=		\
sptoascii.o


sptoascii:	$(OBJ)
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o sptoascii $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


sptoascii.o:		sptoascii.r
	$(SSPP) -r -$(SSPPFLAGS)  sptoascii.r > sptoascii.x
	$(RF) $(SP_RFLAGS) sptoascii.x > sptoascii.f
	$(F77) $(SP_FFLAGS) -c sptoascii.f
	rm -f sptoascii.x sptoascii.f

install:
	mv -f sptoascii $(SP_LOCAL)/bin/sptoascii
	chmod 755 $(SP_LOCAL)/bin/sptoascii
