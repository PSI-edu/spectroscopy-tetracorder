
LD_LIBRARY_PATH='/usr/local/lib /usr/lib/x86_64-linux-gnu'
  export LD_LIBRARY_PATH

# specpr config:

# note: in SSPPFLAGS, set TERMDELAY if you need extra delays by the
# cpu for graphics terminal graphics.
SSPPFLAGS='LINUX -INTEL  -XWIN '
  export SSPPFLAGS
SPECPR=/src/local/specpr
  export SPECPR
RANDRET=32767
  export RANDRET
SP_LOCAL=/usr/local
  export SP_LOCAL
SP_BIN=securebin
  export SP_BIN

# the following is needed for Linux red Hat 8, 9, ....
# but not before. So comment out the one not needed
#
### does not work  LDLIBS '/usr/lib/libc.a -L/usr/X11R6/lib -lX11'
#  SP_LDFLAGS -L/usr/lib64
#  SP_LDLIBS '-L/usr/X11R6/lib -lX11 -lgfortran'
#  SP_LDLIBS '-L/usr/X11R6/lib -lX11'
SP_LDFLAGS=' '
  export SP_LDFLAGS
SP_LDLIBS='-lX11'
  export SP_LDLIBS

#
# The following should be fine as is, except FFLAGS might need
#               floating point accelerator flags
#               (e.g. on sun add -ffpa if you have an accelerator)
#
SPSDIR=syslinux
  export SPSDIR
SPSYSOBJ='${SP_OBJ}/syslinux.o'
  export SPSYSOBJ
RANLIB='ranlib'
  export RANLIB
SSPP='sspp'
  export SSPP
SP_DBG='${SPECPR}/debug'
  export SP_DBG
SP_TMP='${SPECPR}/tmp'
  export SP_TMP
SP_OBJ='${SPECPR}/obj'
  export SP_OBJ
SP_LIB='${SPECPR}/lib'
  export SP_LIB

F77=gfortran
  export F77
CC='cc'
  export CC
AR='ar'
  export AR
RF='ratfor'
  export RF
YACC=yacc
  export YACC
LEX=flex
  export LEX

SP_FFLAGS='-C -O -m64'
  export SP_FFLAGS
SP_FFLAGS1='-C -m64'
  export SP_FFLAGS1
SP_FFLAGS2='-C -m64'
  export SP_FFLAGS2
SPKLUDGE=LINUX
  export SPKLUDGE

# Added to following to not interrupt a backslash
BSLASH='-fno-backslash'
  export BSLASH

SP_FOPT='-O'
  export SP_FOPT
SP_FOPT1='-O'
  export SP_FOPT1
SP_FOPT2='-O'
  export SP_FOPT2
SP_RFLAGS='<'
  export SP_RFLAGS
SP_CFLAGS='-O -m64'
  export SP_CFLAGS
SP_ARFLAGS='rv'
  export SP_ARFLAGS
SP_GFLAGS='-s'
  export SP_GFLAGS
SP_LFLAGS=' '
  export SP_LFLAGS
SP_YFLAGS=' '
  export SP_YFLAGS

# for davinci:
LD_RUN_PATH='/usr/local/lib'
  export LD_RUN_PATH

# these are in gfit/makefile.  If gfit is compiled, may need to do something with these
# GF1_LFLAGS ' '
# LIBPW ' '

