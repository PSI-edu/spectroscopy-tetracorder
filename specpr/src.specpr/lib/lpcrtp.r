	subroutine lpcrtp(index,nlamda,lbnd,tbnd,ay)
	implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language: Fortran
#ccc
#ccc  short description:
#ccc         This subroutine accepts data in real and integer
#ccc         form. to specify in real form index is equal to
#ccc         one, to specify in integer form index is equal
#ccc         to zero. also determines the intensity range and
#ccc         sets headings for plot
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          crtin,wjfren
#ccc  argument list description:
#ccc        arguments: index,nlamda,lbnd,tbnd,ay
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


	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lundefs"
	include "../common/alphabet"

	real*4       x(5)
	real*4       ay(SPMAXCHAN)
	character*72    lbuf
	character*100   ifro

	real*4 lbnd

	ubnd = tbnd
	i = 0
	diwch = 0.
	ifro(9:10) = '  '
#
#     determine vertical scaling and labels
#
	y = ubnd-lbnd
	x(1) = lbnd
	x(2) = lbnd+y*.25
	x(3) = lbnd+y*.50
	x(4) = lbnd+y*.75
	x(5) = ubnd
#
#     determine device labels
#
	if (idv1==ihw)  ifro = iwdgt
	else if (idv1==ihv) ifro = isavt
	else if (idv1==ihd) ifro = iwrkt
	else if (idv1==ihu) ifro = inmu
	else if (idv1==ihy) ifro = inmy
	else if (idv1==ihs) ifro = 'star pack'


10	repeat {
		write(ttyout,20)ititl,ifro,filno,
			ctb(1),ctb(2),ctb(3),
			stb(1),stb(2),ctb(3),
			ihist,dateb,revs
		jl = 1
		call crtin
		call wjfren(jl,x1,il)
		if ( il == ihx || il == ihe ) return
	} until(il==0)
	if (jl>=80) {
		ist = 1
		ifin = nlamda
	}
	if (jl<80) {
		call wjfren(jl,x2,il)
		ist = x1
		if (il!=0) ist = 1
		ifin = x2
		if (il!=0) ifin = x1
	}
	if (ist<=0) ist = 1
	if (ifin>=nlamda) ifin = nlamda
	if (ifin<ist) ifin = ist
	write(ttyout,30)x
	write(ttyout,40)
#
#    determine vertical scaling to fit two pages except for less than
# thirty channels
#
	do i = ist,ifin {
		datav = data(i)
		if (datav!=-1.23e34) xplt = ((datav-lbnd)/y)*72.+0.5
		if (datav==(-1.23e34)) xplt = 1.
		if (xplt<1.) xplt = 1.
		if (xplt>=72.) xplt = 72.
		iplot = xplt
#
#  if index is equal to zero the error bars are set to the same value
# as the plotted point so that they do not appear.
#
		if (index==0) {
			ilerr = iplot
			iuerr = iplot
		} else {
			if (datav!=-1.23e34)
				xuerr = ((datav+error(i)-lbnd)/y)*72.+0.5
			if (datav==(-1.23e34)) xuerr = 1.
			if (xuerr>72.) xuerr = 72.
			if (xuerr<1.) xuerr = 1.
			iuerr = xuerr
			if (datav!=-1.23e34)
				xlerr = ((datav-error(i)-lbnd)/y)*72.+0.5
			if (datav==(-1.23e34)) xlerr = 1.
			if (xlerr>72.) xlerr = 72.
			if (xlerr<1.) xlerr = 1.
			ilerr = xlerr
		}
		lbuf = ' '
		do ii = ilerr,iuerr
			lbuf(ii:ii) = '-'
		lbuf(iuerr:iuerr) = ')'
		lbuf(ilerr:ilerr) = '('
		lbuf(iplot:iplot) = '*'
#
#     determine format and write data
#
		write(ttyout,50)i,lbuf
	}
	write(ttyout,40)
	write(ttyout,70)x
	return

20      format (5x,a,'  data from ',a,'  file',i6,/,
    5x,40('-'),'  civil t=',2(a,':'),a,'  sid t=',2(a,':'),a,/,
    ' history:',a,/,' ch#',43x,'date=',2(a,'/'),a,'     revs=',i6,//,
    ' type starting and ending channels or',/,
    ' return to continue')

30  format(/,1x,f9.3,3(6x,f12.3),4x,f12.3)

40  format(' ',5('-'),4('i-----------------'),'-i')

50  format(' ',1x,i4,'i',a,'i')


70  format(' ',f9.3,3(6x,f12.3),5x,f12.3)

	end
