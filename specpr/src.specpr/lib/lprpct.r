	subroutine lprpct(index,nlamda,lbnd,tbnd,ay)
	implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc       This subroutine accepts data in real and integer
#ccc       form. to specify in real form index is equal to
#ccc       one, to specify in integer form the index is equal
#ccc       zero. also determines the intensity range and sets
#ccc       headings for plot.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc       setspool,dumpspool
#ccc  argument list description:
#ccc       arguments: index,nlamda,lbnd,tbnd,ay
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
	character*100   lbuf
	character*10    ifro

	real*4 lbnd

	call setspo
	ubnd = tbnd
	i = 0
	av = 0
	diwch = 0
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
	if (idv1==ihw) ifro = iwdgt
	else if (idv1==ihv) ifro = isavt
	else if (idv1==ihd) ifro = iwrkt
	else if (idv1==ihs) ifro = 'star pack'
	else if (idv1==ihu) ifro = inmu
	else if (idv1==ihy) ifro = inmy


	write(lstlun,10)ititl,ifro,filno,revs,
		ctb(1),ctb(2),ctb(3),
		stb(1),stb(2),stb(3),
		ihist,dateb,x
#
#     determine vertical scaling to fit two pages except for less than
# thirty channels
#
	if (nlamda<30) nbline = (60-nlamda)/(nlamda-1)
	else nbline = (120-nlamda)/(nlamda-1)

	do i = 1,nlamda {
		av = ay(i)
		datav = data(i)
		xplt = ((datav-lbnd)/y)*100.+0.5
		if (xplt<1) xplt = 1
		if (xplt>=100) xplt = 100
		iplot = xplt
#
#  if index is equal to zero the error bars are set to the same value
# as the plotted point so that they do not appear.
#
		if (index==0) {
			ilerr = iplot
			iuerr = iplot
		} else {
			xuerr = ((datav+error(i)-lbnd)/y)*100.+0.5
			if (xuerr>100) xuerr = 100
			if (xuerr<1) xuerr = 1
			iuerr = xuerr
			xlerr = ((datav-error(i)-lbnd)/y)*100.+0.5
			if (xlerr>100) xlerr = 100
			if (xlerr<1) xlerr = 1
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
		if (datav<99.) write(lstlun,40)av,i,datav,lbuf,i
		if (datav>99.&&datav<=999.) write(lstlun,30)av,i,datav,lbuf,i
		if (datav>999.) write(lstlun,20)av,i,datav,lbuf,i
		if (nlamda!=i)
			if (nbline>=1)
				if (nbline==1)
					write(lstlun,50)
				else
					do k = 1,nbline
						write(lstlun,60)
	}
	write(lstlun,70)
	call dumpsp
	return

10      format (23x,a40,
		'   data from ',a10,
		' file',i6,
		'  revs=  ',i6,/,
		' wave   ch   data',7x,40('-'),'     civil t=',2(a2,':'),a2,
		'   sid t=',2(a2,':'),a2,/,
		' length',17x,'history: ',a60,
		'    date=',2(a2,'/'),a2,/,
		13x,f12.3,12x,2(f12.3,13x),f12.3,11x,f12.3,/,
		2x,19('-'),4('i------------------------'),'i-')

20  format(' ',f6.3,'[',i3,f9.1,'[',a,'[',i3)

30  format(' ',f6.3,'[',i3,f9.3,'[',a,'[',i3)

40  format(' ',f6.3,'[',i3,f9.4,'[',a,'[',i3)

50  format(7x,'[',12x,'[',100x,'[')

60  format(7x,'[',12x,'[',100x,'[')

70 format(2x,19('-'),4('i------------------------'),'i-')

	end
