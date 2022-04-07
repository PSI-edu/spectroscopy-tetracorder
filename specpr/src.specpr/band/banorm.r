	subroutine banorm
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Fortran
#ccc
#ccc  short description:
#ccc         This subroutine band normalizes a spectrum by a
#ccc         least square linear fit to the data points.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc    banmov,movabs,sb,drwabs,crtin
#ccc	wjfren,bandel,banmov,banins
#ccc  argument list description:
#ccc     arguments: none
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

	include "../common/lundefs"
	include "../common/blank"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/alphabet"

	integer*4 idelet(SPMAXCHAN)
	equivalence (idelet(1),datab(1))

	integer*4 delcnt,  dlcnt1
	character*80 outline


#  initialize, write screen

	if (xnrm.eq.0.0) xnrm= 1.0
9	do  i=1,maxchn
		datab(i)=0
	delcnt=0
	dlcnt1=0
15	call banscr(ititl)
#
#  verify band limits
#
	if ((ibnrm1.lt.1).or.(ibnrm1.gt.nchans)) go to 20
	if ((ibnrm2.lt.1).or.(ibnrm2.gt.nchans)) go to 20
	intvl = ibnrm2 - ibnrm1
	nopts = intvl + 1
	if (intvl.lt.0) go to 20
	if (intvl.eq.0) go to 30
	if (intvl.gt.0) go to 40
20	write (outline,21) ibnrm1,ibnrm2,char(0)
	call gwrite(outline)
	call banmov (ibnrm1,ibnrm2,nchans,iopcon)
	if (iopcon(1:1).eq.'x') goto 9000
	go to 15
30	if (idad.ne.2) error(ibnrm1)=0
	ymax=datac(ibnrm1) + error(ibnrm1)
	ymin=datac(ibnrm1) - error(ibnrm1)
	go to 50
#
#  determine max,min, and range for y axis
#
#  is=starting channel for plot, ie=ending channel for plot
#  c1=left edge of graph, c2=right edge; fixed for given band limits
#
40	ymax = -999999.
	ymin = 999999.
	mrgpts = intvl/2
	is = ibnrm1 - mrgpts
	ie = ibnrm2 + mrgpts
	c1 = float(ibnrm1) - float(intvl)/2.
	c2 = float(ibnrm2) + float(intvl)/2.
	if (is.le.1) is = 1
	if (ie.ge.nchans) ie = nchans
	do i=is,ie {
		if (datab(i).eq.1 || datac(i).eq.-1.23e34) next 
		if (idad.ne.2) error (i)=0
		ymx = datac(i) + error(i)
		ymn = datac(i) - error(i)
		if (ymx.gt.ymax) ymax=ymx
		if (ymn.lt.ymin) ymin=ymn
	}
50	if (ymax.lt.ymin) ymax=ymin+0.001
	ymax = ymax * 1.03
	ymin = ymin * .97
	if (ymin.lt.0) ymin = ymin * 1.062
	rng = ymax - ymin
	if (rng.lt.0.1e-35) rng=0.1e-35
	if (rng.gt.0.1e+36) rng=0.1e+36
	call movabs(173*2,308*2)
	call sb(0)
	write (outline,52) ymax,char(0)
	call gwrite(outline)
	call movabs(173*2,124*2)
	call sb(0)
	write (outline,52) ymin,char(0)
	call gwrite(outline)
	if (nopts.eq.1) go to 62
#
#  accumulate sums for least squares analysis
	xn = 0
	x = 0
	y = 0
	xy=0
	x2=0
	y2=0
	do i=ibnrm1,ibnrm2 {
		if (datab(i).eq.1 || datac(i).eq.-1.23e34) next 
		xn=xn+1
		x=x+i
		y=y + datac(i)
		xy=xy + i*datac(i)
		x2=x2 + i**2
		y2=y2 + datac(i)**2
	}
62	if (nopts.ne.1) go to 64
#
#  if only one data point, draw & label tick mark, set constants
#
	call movabs(377*2,122*2)
	call drwabs(377*2,119*2)
	call movabs(363*2,113*2)
	call sb(0)
	write (outline,68) ibnrm1,char(0)
	call gwrite(outline)
	a=0.
	b=0.
	r=0.
	xn=1.
	is=ibnrm1-1
	ie=ibnrm2+1
	c1=is
	c2=ie
	k=7
	go to 74
#
#  calculate least squares solution and correlation coefficient
#
64	delta = xn*x2 - x**2
	if (abs(delta).le.0.1e-30) delta=0.1e-30
	a = (x2*y - x*xy)/delta
	b = (xn*xy - x*y)/delta
	if (abs(b).lt.0.1e-35) b=0.1e-35
	xndy= delta*(xn*y2- y**2)
	if (abs(xndy).lt.0.1e-30) xndy= 0.1e-30
	r2=(xn*xy - x*y)**2/xndy
	if (r2.lt.0) {
		write(outline,'(a1)') char(0)
		call gwrite(outline)
		call gwrite(outline)
		write (outline,65) char(0)
		call gwrite(outline)
	}
	if (r2.lt.0) r2=0
	r=sqrt(r2)
#
#  determine tick interval for x-axis
#  xint = interval value  (x1 not used)
#
66	bnrm1=ibnrm1
	bnrm2=ibnrm2
	call inice(bnrm1,bnrm2,xint,10.0,x1)
	if (xint.lt.1.) xint=1.
	m=0
#
#  draw tick marks and label them
#
	do i=1,10 {
		m=m+1
		itick = ibnrm1 + (i-1)*(xint)
		if (itick.gt.ibnrm2) go to 72
		isc = 313 + (i-1)*xint * 128/intvl +0.5
		call movabs (isc*2,122*2)
		call drwabs (isc*2,119*2)
		call movabs ((isc-20)*2,105*2)
		call sb(0)
		if (m.eq.1) {
			write (outline,68) itick,char(0)
			call gwrite(outline)
		}
		if (m.eq.2) m=0
	}
#
#  plot the points. 2k determines size of plus
#
72	k=2
	if (nopts.lt.40) k=3
	if (nopts.lt.20) k=4
74	do i=is,ie {
		if (datab(i).eq.1) next
		y = (datac(i) - ymin)*184./rng +0.5
		y=y + 122*2
		c=i
		x=255.*2.0*(c-c1)/(c2-c1)
		x= x+0.5
		x= x + 249*2
#
#  check that x & y are within screen boundaries
		if (y.gt.306.) y=306.*2.0
		if (y.lt.122.) y=122.*2.0
		if (x.gt.504.) x=504.*2.0
		if (x.lt.0.) x=0.
		ix=x
		iy=y
#
#  draw a plus of width 2k at point (ix,iy)
#
		call movabs(ix,iy+k)
		call drwabs (ix,iy-k)
		call movabs (ix+k,iy)
		call drwabs (ix-k,iy)
#
#  draw error bar
#
		if (error(i).eq.0) next
		ir = error(i) * 184.*2.0/rng +0.5
		if (ir.gt.1000) ir=1000
		call movabs (ix+k,iy+ir)
		call drwabs (ix-k,iy+ir)
		call movabs (ix, iy+ir)
		call drwabs (ix,iy-ir)
		call movabs (ix+k,iy-ir)
		call drwabs (ix-k,iy-ir)
	}
	if (nopts.eq.1) y=datac(ibnrm1)
	if (nopts.eq.1) go to 90
#
#  draw the line, checking that it stays within the box
#
	x = ibnrm1
	ix = 313*2
	y = a + b*x
	if (y.gt.ymax) go to 81
	if (y.lt.ymin) go to 82
	go to 84
81	y=ymax
	go to 83
82	y=ymin
83	x=(y-a)/b
	ix=(x-c1)/(c2-c1)*255.*2.0 + 249.5*2.0
84	y = (y - ymin)*184.*2.0/rng
	iy = y + 122*2
	call movabs (ix,iy)
	x = ibnrm2
	ix = 440*2
	y=a + b*x
	if (y.gt.ymax) go to 85
	if (y.lt.ymin) go to 86
	go to 88
85	y=ymax
	go to 87
86	y=ymin
87	x=(y-a)/b
	ix=(x-c1)/(c2-c1)*255.*2.0+ 249.5*2.0
88	y = (y - ymin)*184.*2.0/rng
	iy = y + 122.*2.0
	call drwabs (ix,iy)
#
#  find value of equation at the center of the band
#  calculate normalization factors
	xctr = float(ibnrm1) + float(intvl)/2.
	y = a + b*xctr
90	call movabs (0,78*2)
	call sb(0)
	if (abs(y).lt.0.1e-30) go to 95
	xnrm1 =y
	xnrm2 = xnrm * y
	go to 100
#
#  process underflow of normalization factor
#
95	call movabs (0,175*2)
	call sb (0)
	write (outline,91) char(0)
	call gwrite(outline)
	write(outline,'(a1)') char(0)
	call gwrite(outline)
	write (outline,92) char(0)
	call gwrite(outline)
	call crtin
	i=1
	call wjfren (i,t,it)
	if (it.eq.ihd) go to 114
	if (it.eq.ihm) go to 116
	if (it.eq.ihi) go to 115
	if (it.eq.ihe) goto 9000
	go to 15
#
#  write stuff
#
100	write(outline,190) r,char(0)
	call gwrite(outline)
	write(outline,195) char(0)
	call gwrite(outline)
	write(outline,191) a,b,char(0)
	call gwrite(outline)
	write(outline,195) char(0)
	call gwrite(outline)
	write(outline,192) xnrm,char(0)
	call gwrite(outline)
	write(outline,193) xnrm1,char(0)
	call gwrite(outline)
	write(outline,194) xnrm2,char(0)
	call gwrite(outline)
#
#  position cursor and accept user instructions
#
	call movabs (0,193*2)
	call sb (0)
105	call crtin
	i=1
	call wjfren (i,t,it)
	if (nopts.eq.1) go to 110
	if (it.eq.ihd) go to 114
	if (it.eq.ihi) go to 115
110	if (it.eq.ihm) go to 116
	if (it.eq.ihe) goto 9000
	if (it.eq.ihb) go to 120
	if (it.eq.ihu) go to 118
	write (outline,113) char(0)
	call gwrite(outline)
	go to 105
114	call bandel(ibnrm1,ibnrm2,nopts,delcnt,iopcon)
	if (delcnt.eq.maxchn) go to 9
	go to 15
115	call banins(ibnrm1,ibnrm2,delcnt,iopcon)
	if (delcnt.eq.maxchn) go to 9
	go to 15
116	call banmov (ibnrm1,ibnrm2,nchans,iopcon)
	if (iopcon(1:1).eq.'1') goto 9000
	go to 15
#
#     unnormalize by setting y to 1./xnrm then normalize
#
118	y = 1./xnrm
#
#     following equation to show possible round off error
#
	xnrm2 = xnrm * y
#
#  normalize
#
120	do i=1,maxchn {
		x= datac(i)/y
		if (datac(i).eq.-1.23e34) x= -1.23e34
		datac(i)= x
		if (idad.ne.2) next 
		error(i) = error(i)/y
	}
	xnrm = xnrm2
	ifut(1) = ibnrm1
	ifut(2) = ibnrm2
9000	continue
	return
21	format (' band limits invalid (',i5,',',i5,')',a1)
52	format (1x,g12.5,a1)
65	format (' r**2 is negative',a1)
68	format (i5,a1)
91	format (' normalization factor too small',a1)
92	format (' enter d, m, i, or e ',a1)
190	format (27x,'correlation coefficient: ',g12.5,a1)
191	format(27x,'line:     y = ',g12.5,'  +  (',g12.5,') x',a1)
192	format(27x,'normalization factor:  previous = ',g12.5,a1)
193	format(27x,24x,'current = ',g12.5,a1)
194	format(27x,25x,'future = ', g12.5,a1)
195	format(' ',a1)
113	format (' **error - reenter**',a1)
	end
