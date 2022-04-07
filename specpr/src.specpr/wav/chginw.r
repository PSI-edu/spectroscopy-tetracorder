	subroutine chginw(ifil)
	implicit integer*4 (i-n)
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#
#-----------------------------------------------------------------------
#  this routine prompts for header information for the wavelength
#  files.
#-----------------------------------------------------------------------
#
# Author:       RKK ????????
# modified:     JAH 01/04/83    to call crtin instead of 'read(5'
#
#----------------------------------------------------------------------
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lblg"
	include "../common/lbl4"
	include "../common/alphabet"
	include "../common/lundefs"

	character*2 istrg

	write(ttyout,10)
	write(ttyout,20)
	call crtin
	i=1
	call wjfren(i,x,il)
	if (il==ihx) return
	write(ttyout,30)
	call crtin
	ititl = iopcon
	write(ttyout,40)
	call crtin
	ihist = iopcon
45      write(ttyout,50)       #type in date
	call crtin
	i=1
	call wjfren(i,x,il)
	if (il!=0) go to 45
	if (i>=80) go to 46
	ix = x
	if ((ix < 1) .or. (ix >12)) {    #month out of range
		call what (i)
		write (ttyout,70)
		go to 45
	}
	write (istrg,'(i2)') ix
	datea(1)(1:2)= istrg
	write (istrg,'(i2)') ix
	dateb(1)(1:2)=istrg
	call wjfren(i,x,il)
	if (il!=0) go to 45
	ix = x
	if ((ix < 1) .or. (ix >31)) {     #day out of range
		call what (i)
		write (ttyout,70)
		go to 45
	}
	write (istrg,'(i2)') ix
	datea(2)(1:2)=istrg
	write (istrg,'(i2)') ix
	dateb(2)(1:2)=istrg
	call wjfren(i,x,il)
	if (il!=0) go to 45
	ix = x
	if ((ix < 1) .or. (ix >100)) {    #year out of range
		call what (i)
		write (ttyout,70)
		go to 45
	}
	write (istrg,'(i2)') ix
	datea(3)(1:2)=istrg
	write (istrg,'(i2)') ix
	datea(3)(1:2)=istrg
46      write(ttyout,60)
	call crtin
	mhist(1:74) = iopcon
	call crtin
	mhist(75:148) = iopcon
	call crtin
	mhist(149:222) = iopcon
	call crtin
	mhist(223:296) = iopcon
47      do i=1,3 {
	    ira(i) = 0
	    idec(i) = 0
	}
   	do i=1,3 {
	    cta(i) = ' '
       	    ctb(i) = ' '
	    sta(i) = ' '
	    stb(i) = ' '
	}	
	revs=1
	filno=ifil
	irmas=0
	itimch=0
	nruns=1
	ieros=0
	iwtrns=1
	xnrm=1
	scatim=1
	timint=1
	return

10      format(' ***** header information *****',/)
20      format('  return  to continue,    x  to exit.',/)
30      format(' type in the title',/,1x,39(1h-),1hi,/)
40      format(' type in the history',/,1x,59(1h-),1hi,/)
50      format(' type in the date',/)
60      format(' type in the manual history (4 lines max)',
	     /,1x,72(1h-),1hi,/)
70	format(' date out of range',/)
100     format(6i2)
103     format(a)
110     format(a)
111     format(a)
114     format(a)
	end
