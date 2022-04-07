	integer function gtrecs(ii)
	implicit integer*4 (i-n)

#ccc  version: @(#)gtrecs.r	2.15 6/18/87 16:25:17
#ccc  author(s): J.A. Hoover
#ccc  language: RATFOR
#ccc
#ccc  short description: 
#ccc		this routine reads the spectrum file and plotting
#ccc        option from the user
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: 
#ccc		wjfren,devlun,devsta,option,savdta
#ccc  argument list description: 
#ccc		ii --- dummy argument
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/pltcnt"
	include "../common/lundefs"
	include "../common/lbl7"
	include "../common/alphabet"
	include "../common/label1"
	include "../common/iocontrol"

	integer*4 prot,status
	integer*4	option
	real*4	x
        real*4 datoff     # data offset
        real*4 datmul     # data multiplier
        real*4 xoffset    # xoffset offset

	write (ttyout,100) maxchn
	write (ttyout,101)
	write (ttyout,110)

	while (nplots<50) {
		do i=1,maxchn {
			ldel(i) = 0
		}
		filid = 0
		recno = 0
		wavid  = itrol(1)
		wavrec = itrol(2)
		nolim = .false.
		newplt = .false.
		linetp = 1
		ptsize = -1
		delete = .false.
		conct = 0
		errbar = 0
		more = 0
		txtflg = .false.
		symtyp = 0
		datoff=0.0
		datmul=1.0
		xoffset = 0.0
		chans = 0
		call crtin
		i = 1
		call wjfren(i,x,il)


		if (il == ihe | il == ihx) {
			gtrecs=-1
			return

		} else if (il == ihr) {
			write (ttyout,200) nplots
			if (nplots>0) nplots = nplots - 1

		} else if (il == ihb) {
			write (ttyout,500) 
			gtrecs=nplots
			return
		} else {
			iflnm = il
			call devlun(4,il,filid)
			call devsta(filid,status,0,prot)
			if (status<=0 || filid==0) {
				write(ttyout,300)
				next
			}
			call wjfren(i,x,il)
			if (il==ihx || (x==0 && il==ihe)) {
				gtrecs=-1
				return
			}
			if (i>=80) {
				write(ttyout,300)
				next
			}
			recno = x
			if (recno>iabs(prot) && prot!=-1) {
				write(ttyout,300)
				next
			}
#
#                       the following is to get wavelength pointer
#                           if needed
#
			call devok (4,iflnm,recno,filid,ier)
			if (ier != 0) {
				write (ttyout,300)
				next
			}
			itmp = recno
			call redfil (itmp,filid,ier)
			if (ier != 0) {
				write (ttyout,300)
				next
			}
			idwcon = iflnm
#
			if (il!=0) i = i - 1
			ii = option(i,datoff,datmul,xoffset)
			if (ii==-1) {
				gtrecs=-1
				return
			}
			if (ii!=0) next
			nplots = nplots + 1
			call savdta(nplots,iflnm,datoff,datmul,xoffset)
		}
		write(ttyout,400)
	}
	gtrecs=nplots
	return

100     format(' Type in the file id (v, w, d, u or y) ',
		'and the record ',
		'number and the options:',/,
'   c = CONNECT POINTS if wavelengths increasing.',/,
'   C = CONNECT POINTS ALWAYS',/ ,
'   e = Include ERRORS',/,
'   V,W,D,U or Y = Wavelength file id + rec. ',
				'number not equal to default ',/,
'   d = DELETE POINTS (',i4,' max), type in the channels to be deleted',/,
'       only when input is requested.  When all ',
			'are typed in TYPE c to CONTINUE.')
101	format (' ',
 '  p = POINT SIZE followed by the number 0 to 5', /,
'   l = LINE COLOR followed by the number 1 to 8',/,
'   L = LINE TYPE followed by the number 1 to 6',/,
'   t = TEXT on graph', /,
'   n = NO PLOTTING of points when > limits', /,
'   s = SYMBOL TYPE followed by the number 1 to 15',/,
'   B = Symbols in Black (default is line color)',/,
'   m = MORE COPIES',/,
'   g = NEW PLOT after this spectrum'/)
110		format(' 50 spectra total may be plotted.  ',
			'After the word CONTINUE',/ ,
' has been written, type in another ',
			'file id and number or',/,
' Type  b  to BEGIN PLOT, or  x  to ',
			'EXIT routine WITHOUT PLOTTING.',// ,
' CONTINUE')

200	format(' ** RETURN to',i5,';  continue'/)

300	format(' INCORRECT ENTRY: Retype',45('-'))

400	format(' CONTINUE')

500	format(' BEGIN PLOT ')
	end
