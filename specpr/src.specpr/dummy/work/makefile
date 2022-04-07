# %A%

SHELL=/bin/sh
GET=sp_if_get

OBJ=	\
f26.o	\
f27.o	\
f28.o	\
f29.o

$(SP_OBJ)/dummy.o:	$(OBJ)
	ld $(LFLAGS) -r -o $@ $(OBJ)
	ar rlv ../specpr.a $@

.SUFFIXES:
.SUFFIXES: .o .r
	
f26.o	work/f26.r:
	ratfor $(RFLAGS) work/f26.r >$(SP_DBG)/f26.f
	f77 $(FFLAGS) -c $(SP_DBG)/f26.f
	
f27.o	work/f27.r:
	ratfor $(RFLAGS) work/f27.r >$(SP_DBG)/f27.f
	f77 $(FFLAGS) -c $(SP_DBG)/f27.f
	
f28.o	work/f28.r:
	ratfor $(RFLAGS) work/f28.r >$(SP_DBG)/f28.f
	f77 $(FFLAGS) -c $(SP_DBG)/f28.f
	
f29.o	work/f29.r:
	ratfor $(RFLAGS) work/f29.r >$(SP_DBG)/f29.f
	f77 $(FFLAGS) -c $(SP_DBG)/f29.f
