	subroutine mulrun (ic,cx,iprodp)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc      This subroutine performs the multiplication functions.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,whedr2,crtin,wjfren,ercomp,whistr,redfil
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
#################################################################
#                                                               #
#       this routine performs the multiplication function       #
#                                                               #
#       AUTHOR Roger N Clark                                    #
#       Modified JAH 04-12-83   convert to ratfor               #
#################################################################

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


#
#     this routine multiplies two files
#
	call eralph
	call whedr2
	write(ttyout, 1)
	write(ttyout, 2) ititl1, revs1, idv1, ifl1
	write(ttyout, 7)
	if(idv2==ihc)
		write(ttyout,20) cx
	else
		write(ttyout,8) ititl2,revs2,idv2,ifl2
	if (iprodp!=1) {
		write(ttyout,3)
		call crtin
		ii = 1
		call wjfren(ii,x,ic)
	}
	if (ic==ihx | ic==ihe) return
	xnrm1 = xnrma
	xnrm2 = xnrmb
#
#     perform multiplication
#
	do j= 1, nchans
		error(j)= 0.0
	do j= 1, nchans
		if(dataa(j)==-1.23e34 | datab(j)==-1.23e34) datac(j)= -1.23e34
		else datac (j)= dataa(j)*datab(j)
	airmas= irmasa/1000.
	if (ictrl==ihe) {
#
#     calculate errors if requested
#
		i=1
		call ercomp(i,ierr,cx)
		if(ierr==ihe) {
			ic =ihe
			return
		}
	}

# read first data set to restore header of first

	irecx = ifl1
	call devlun (0, idv1, il1)
	call redfil (irecx, il1, ier)
	if (ier != 0) {
		ic = ihe
		return
	}

	xnrm= xnrm1 * xnrm2

	mhist= ' '    # blank out mhist

	if (idv2==ihc)
		iwtrns= iwtrna
#
#     decode history
#
	call whistr("*",cx)
	return

1       format (1x, 20x, 'multiplication of two files routine', /)
2       format ( 10x, 'form:', //, 2x, a, 2x, '# revs=', f5.0,
		2x, 'file:', 1x, a, i5)
3       format (10x,'press return to continue or type  e  to exit routine.')
7       format (19x, 5(1h*), /, 19x, 'times', /, 19x, 5(1h*) )
8       format (2x, a, 2x, '# revs=', f5.0, 2x, 'file:', 1x, a, i5, /)
20      format (' the constant',f15.7)
	end
