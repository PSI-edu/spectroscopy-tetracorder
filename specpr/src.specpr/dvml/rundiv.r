	subroutine rundiv(ic,cx,iprodp)
	implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine does division of two files.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          er,whedr2,reastr,wjfren,ercomp, whistr, redfil
#ccc  argument list description:
#ccc     arguments: ic,cx,iprodp
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
####################################################################
#     this subroutine divides two files and performs extinction
#     corrections with a standard
####################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"



	call eralph
	call whedr2
	is1 = 1
	xnrm1 = xnrma
	xnrm2 = xnrmb
	write(ttyout,40)
	write(ttyout,50)ititl1,revs1,idv1,ifl1
	write(ttyout,60)
	if (istarp(1)==ihs) {
#
#     read in starpack if requested
#
		if1 = ifl1
		if2 = istarp(2)
		irv1 = revs1
		ifil = istarp(2)
		ifl2 = istarp(2)
		go to 20
	} else if (idv2!=ihc) write(ttyout,100)ititl2,revs2,idv2,ifl2
	else {
		write(ttyout,90)cx
		go to 10
	}

	repeat {
		if (revs1!=revs2) write(ttyout,110)
		if (istarp(1)==ihs) write(ttyout,120)ititl1(17:40)
10		if (iprodp!=1) {
			write(ttyout,130)
			call crtin
			ii = 1
			call wjfren(ii,x,ic)
		}
		if (ic!=ihs) break 1
		repeat {
			write(ttyout,160)
			call crtin
			i = 1
			ifil = 0
			call wjfren(i,x,il)
		} until(il==0&&x!=0.)
		istarp(2) = x
		ifl2 = x
		ifil = x
20		ifx = ifilex
		call reastr(ier,ifl2)
		if (ier!=0) {
			write(ttyout,70) ier,ifl2
			call crtin
			ifilex = ifx
			ifl1 = if1
			ifl2 = if2
			revs1 = irv1
			ic = ihe
			return
		}
		ifilex = ifx
		istarp(2) = ifl2
		write(ttyout,80)istarp,revs1
		ifl1 = if1
		ifl2 = if2
		xnrm2 = xnrma
		revs2 = revs1
		revs1 = irv1
		ihistc =  ':' // ititl1(17:40)
	}
	if (ic!=ihx) {
		if (ic==ihe) write(ttyout,140)
		if (ic!=ihe) {
			if (istarp(1)==ih0&&istarp(2)==ih0) {
#
#     this section performs a regular division of two files
#
				do j = 1,nchans {
				    error(j) = 0.0
				    if (datab(j)!=-1.23e34&&dataa(j)!=-1.23e34)
					if (abs(datab(j))>=0.1e-35) {
					    datac(j) = dataa(j)/datab(j)
					    next 1
					}
					datac(j) = -1.23e34
				}
				irmas = irmasa
				if (ictrl==ihe) {
					i = 1
					call ercomp(i,ierr,cx)
					if (ierr==ihe) {
						ic = ihe
						return
					}
				} else {

				    irecx = ifl1
				    call devlun (0, idv1, il1)
				    call redfil (irecx, il1, ier)
				    if (ier != 0) {
					ic = ihe
					return
				    }

				    airmas = irmas/1000.
				}
			} else {
				idv2 = ihs
				write(ttyout,150)
				idv2 = ihs
				airmas = irmas/1000.
				do i = 1,nchans {
					dataa(i) = data(i)
					b = datab(i)*airmas
					if (b>=37.0) b = 37.0
					if (datac(i)+b>=37.0) datac(i) = 37.0-b
					a = (10.**datac(i))*(10.**b)
					if (abs(a)<0.1e-30) datac(i)=9.9999e+30
					if (abs(a)>0.1e-30) {
						datac(i) = dataa(i)/a
						if (abs(datac(i))<=1.e-20)
							datac(i) = 0.0
					}
				}

				irecx = ifl1
				call devlun (0,idv1, il1)
				call redfil (irecx, il1, ier)
				if (ier != 0) {
					ic = ihe
					return
				}

				airmas = irmas/1000.
			}
			if (abs(xnrm1) < 0.1e-36) {
				write (ttyout,171) xnrm1
				xnrm1 = 1.0
			}
			if (abs(xnrm2) < 0.1e-36) {
				write (ttyout,172) xnrm2
				xnrm2 = 1.0
			}
			xnrm = xnrm1/xnrm2

#     blank out manual history

			mhist = ' '
#
#     determine history
#
			call whistr("/",cx)
		}
	}
	return

40  format(1x,30x,'division routine',/)
50  format(10x,'form:',//,a,'  # revs=',f5.0,' file: ',a,i5)
60  format(19x,10('*'),/,19x,'divided by',/,19x,10('*'))
70  format(1x,'I/O error',i5,' on starpack file, record',i3,/,
		'PRESS RETURN TO EXIT')
80  format(1x,'star pack=',a,i4,10x,'revs=',f5.0)
90  format(1x,'the constant',f15 .7)
100  format(a,'  # revs=',f5.0,' file: ',a,i5,/)
110  format(1x,'note: the number of revolutions of each data run are not equal')
120  format(1x,a,10x,'type  s  to change starpack number')
130  format(10x,'press return to continue or type  e  to exit routine.')
140  format(20x,'routine aborted',//)
150  format(1x,'*** working ***')
160  format(1x,'type in starpack number'/)
171  format(1x,'WARNING! normalization factor in ',
		'numerator is near zero', /, 10x,
		'was:', 0pe12.6, '   resetting to 1.0 to avoid',
		' floating underflow')
172  format(1x,'WARNING! normalization factor in ',
		'denominator is near zero', /, 10x,
		'was:', 0pe12.6, '   resetting to 1.0 to avoid',
		' floating overflow')
end
