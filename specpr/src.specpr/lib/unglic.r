	subroutine unglic(ig)
	implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine determines what may be gliches
#ccc         and attempts to remove those which the user wants
#ccc         to remove
#ccc         note: this routine can only be used on raw or
#ccc         subtracted data
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          sb,maw,vaw,crtin,wjfren
#ccc  argument list description:
#ccc        argument: ig
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
######################################################################
#     this subroutine determines what may be gliches and attemps to
#     remove those which the user wants to remove
#
#     note: this routine can only be used on raw or subtracted data
######################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"

	integer*4       idax(15)


	diff = ubnd-bbnd
	mx = 0
	n = 2
	dmax = -99999.
	dmin = 0
#
#     determine the data value range and search for gliches which are
#     greater than 6 percent of this value
#
	do i = 1,maxchn {
		if (datac(i)>dmax) dmax = datac(i)
		if (datac(i)<dmin) dmin = datac(i)
	}
	dmmx = (dmax-dmin)*0.06
	dmmx = abs(dmmx)
	repeat {
		nn = n*2
		if (n<dmmx&&nn>dmmx) break 1
		n = n*2
	} until(n>=2**14)
	do i = 1,15
		idax(i) = 0
	j = 1
#
#     check 4 conditions for each data point to determine if it is a
#     glich
#
	do i = 4,118
		if (i<59||i>62) {
			a = (datac(i-1)+datac(i+1))/2.
			b = (datac(i-1)-datac(i-2))*2+datac(i-2)
			c = (datac(i+2)-datac(i+1))*(-1)+datac(i+1)
			d = b+c/2
			nxy = 0
			if (abs(a-datac(i))>n) nxy = nxy+1
			if (abs(b-datac(i))>n) nxy = nxy+1
			if (abs(c-datac(i))>n) nxy = nxy+1
			if (abs(d-datac(i))>n) nxy = nxy+1
			if (b<=datac(i)&&datac(i)<=c) nxy = 0
			if (c<=datac(i)&&datac(i)<=b) nxy = 0
			if (nxy==4) {
				idax(j) = i
				j = j+1
				if (j>15) break 1
			}
		}
	j = j-1
	do i = 1,j {
		m = idax(i)
		if (m>=1&&m<=120) {
			iy = ((datac(m)-bbnd)/(diff))*1000+200
			if (iy>1200) iy = 1200
			if (iy<200) iy = 200
			ix = 200+m*7.
			jx = 10
#
#     identify points which are calculated to be gliches
#
			call sb(0)
			call maw(ix,iy+jx)
			call vaw(ix-jx,iy)
			call vaw(ix,iy-jx)
			call vaw(ix+jx,iy)
			call vaw(ix,iy+jx)
		}
	}
#       il is the channel division interval
	i1=5
	if (nchans > 256) i1=10
	i2 = nchans/i1
	do i = 1,i2 {
		xi = i
		x = 112.0+xi*(i1*(1000.0-112.0)/nchans)+0.5
		ix = x
		call movabs(ix,552)
		call drwabs(ix,92)
	}
	call maw(0,780)
	call sb(0)
	write(ttyout,40)n,idax

	call crtin
	i = 1
	call wjfren(i,x,il)
	mga = 0
#
#     decode operator decision on gliches
#
	if (il!=ihe) {
		if (il!=ihb) {
			m = 0
			if (il!=iha) {
				if (il==iho) j = 0
				if (il!=iho) go to 30
				do ij = 1,15
					idax(ij) = 0
				do ij = 1,15 {
					call wjfren(i,x,il)
					if (i>=80) go to 10
					idax(ij) = x
				}
			}
			call wjfren(i,x,il)
			if (i>=80) go to 10
			do ij = 1,15 {
				it = x
				if (idax(ij)==it)
				idax(ij) = 0
			}
		}
		repeat {
			call wjfren(i,x,il)
			if (i>=80) break 1
			it = x
			do ij = 1,15 {
				if (idax(ij)==0) idax(ij) = it
				if (idax(ij)==it) go to 900
			}
			break 1
900             continue
		}
#
#     remove gliches (assumed to be some power of 2)
#
10		do i = 1,15 {
			c = 128
			m = idax(i)
			if (m>=2&&m<=119) {
				a = (datac(m-1)+datac(m+1))/2.
				b = datac(m)-a
				d = abs(b)
				repeat {
					c = c*2.
					if (c>2.**24) go to 20
					if (c<0.000002) go to 20
					e = 3.*c/4.
					f = 3.*c/2.
					if (d>e&&d<=f) break 1
					if (d<=f && d<=e) c = c/4.
				}
				if (b<0) datac(m) = datac(m)+c
				if (b>0) datac(m) = datac(m)-c
				data(m) = datac(m)
			}
		}
		write(ttyout,50)
		go to 30
20		write(ttyout,60)
		ig = 0
		return
	}
30	ig = 0
	return

40	format (' possible gliches larger than',i6,' in channels:',/,
		2x,15i4,/,' type in channels to be degliched.  ',
		'options: a= all plus n1,n2,','...;',/,
		' b= all except n1,n2,...; o= only n1,n2,...; ',
		'e= exit routine',/)

50	format (' finished',70('-'))

60	format (' warning... value out of bounds of the routine')
	end
