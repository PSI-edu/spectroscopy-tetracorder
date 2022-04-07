	integer*4 function manhst(inm,i1,ids,ifls,i2,mhist,iflag,cx)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc         This is manual history routine routine. the
#ccc         function value returned is the number of members
#ccc         of the array mhist actually used.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    namdev,comprs
#ccc  argument list description:
#ccc     arguments: inm,il,ids,ifls,i2,mhist,iflag,cx
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
#   manual history routine
#   inm     -  array containing name of calling function. string length i1
#   ids     -  array containing file ids.
#   ifls    -    "        "       "   " .
#   i2      -  number of elements used in ids and ifls.
#              arrays ids and ifls contain, as their first element,
#              the first file to which the calling function is
#              writing. the other (i2-1) elements are the files to be
#              written into the manual history.
#   mhist   -  the manual history.  dimensioned 148.
#   mhista  -  working array for manhst.  dimensioned 296.
#   iflag   -  set to 0 in calling program before first call. after
#              a call to manhst, it has the following value:
#              = n  wrote n'th manual history.
#              = -n wrote n'th and last manual history.
#   cx         = constant (real)
#   the function value returned is the number of members of the array
#   mhist actually used.

	include "../common/alphabet"
	include "../common/lundefs"

#RED
	integer*4 lnb      # function lnb
	
	character*296	mhist
	character*(*)	inm
	character*8		work1

	integer*4		ids(i2),ifls(i2),to

	iv = 0
	iu = 0
	id = 0
	iw = 0
	iy = 0
	ic = 0

	do i=1,i2 {
		if (ids(i) == ihv) iv = iv + 1
		if (ids(i) == ihy) iy = iy + 1
		if (ids(i) == ihu) iu = iu + 1
		if (ids(i) == ihw) iw = iw + 1
		if (ids(i) == ihd) id = id + 1
		if (ids(i) == ihc) ic = ic + 1
	}

	mhist = inm // ':'

	if (iv !=0) {
		call namdev(ihv,work1)
		write (mhist,1000) mhist(1:lnb(mhist)),ihv,work1
	}
	if (iw !=0) {
		call namdev(ihw,work1)
		write (mhist,1000) mhist(1:lnb(mhist)),ihw,work1
	}

	if (id !=0) {
		call namdev(ihd,work1)
		write (mhist,1000) mhist(1:lnb(mhist)),ihd,work1
	}

	if (iu !=0) {
		call namdev(ihu,work1)
		write (mhist,1000) mhist(1:lnb(mhist)),ihu,work1
	}

	if (iy !=0) {
		call namdev(ihy,work1)
		write (mhist,1000) mhist(1:lnb(mhist)),ihy,work1
	}

	if (ic !=0) {
		write (mhist,1001) mhist(1:lnb(mhist)),ihc,cx
	}


	for (i = 1; i <= i2; i = i+1) {
		if (ids(i) == ihc) next
		call comprs(mhist)

# FUTURE: this increment of 1 only works when the nchans is <= 256,
#         otherwise, the continuation records will make increment
#         greater than 1.  So pass nchans and figure out the increment.

		to = i+1

		while (to<=i2 && ids(i)==ids(to) && (ifls(i)-i)==(ifls(to)-to) )
		    to = to + 1

		to = to - 1
		n = index(mhist,' ')

		if (n == 0) {
			write (ttyout,*) 'WARNING: mhist array full, ',
					' some history deletet'
			n = 296
			manhst = n
			return
		}

		if (to>=i+2) {
			write(mhist(n:296),2000) ids(i),ifls(i),ids(to),ifls(to)
			i = to
		} else  {
			write(mhist(n:296),3000) ids(i),ifls(i)
		}

	}
	call comprs(mhist)
	n = lnb(mhist)
	mhist(n:n) = ' '
	manhst = n-1
	return

1000	format (a,a1,'=',a8,';')
1001	format (a,a1,1pe12.5,',')
2000	format (a1,i6,'to',a1,i6,',')
3000	format (a1,i6,',')

	end
