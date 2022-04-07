
DBG=../debug

OBJ=		\
	$(SP_LIB)/sputil.a


all:		oceanopticstospecpr

install:	oceanopticstospecpr
		cp oceanopticstospecpr /usr/local/bin/oceanopticstospecpr
		size /usr/local/bin/oceanopticstospecpr
		cp oceanopticstospecpr-multiple /usr/local/bin/

oceanopticstospecpr:	oceanopticstospecpr.o $(OBJ)
		$(F77) $(SP_FFLAGS) -o oceanopticstospecpr oceanopticstospecpr.o \
			$(OBJ) $(SP_LIB)/specpr.a $(SP_LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r

oceanopticstospecpr.o:	oceanopticstospecpr.r
		sspp -r -$(SSPPFLAGS) oceanopticstospecpr.r > oceanopticstospecpr.x
		$(RF) $(SP_RFLAGS) oceanopticstospecpr.x > oceanopticstospecpr.f
		rm oceanopticstospecpr.x
		$(F77) $(SP_FFLAGS) -c  oceanopticstospecpr.f
		rm oceanopticstospecpr.f


