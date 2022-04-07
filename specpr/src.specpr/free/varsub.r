	function varsub(idummy)

	implicit integer*4 (i-n)
	integer*4 varsub
########################################################################
#                                                                      #
#      this routine performs variable substitution on the              #
#      free format input string.                                       #
#                                                                      #
#      forms like '$24' are replaced from the command file entries     #
#                                                                      #
########################################################################
	include "../common/lbl4"
	include "../common/cmd"
	include "../common/lundefs"
	include "../common/iocontrol"

#RED
	integer*4 ihchar     # function ihchar

	character*80 istr,work,work2
	character*1 icnull
	character*2 bckslpnd

	logical smfnd
	integer*4 comma
	integer*4 ier,idummy,name
	real*4 x,c,a

#05/30/2002  TEST
#RED Intialize to 0
	iname = 0
	in = 0

	comma = ihchar(',')
	bckslpnd(1:1)=char(92)  # this is the backslash character
	bckslpnd(2:2)=char(35)  # this is the pound character
	icnull = char(0)
	work = ' '
	work2 = ' '

	subcnt = 0

10	do i = 80, 2, -1 {
		if (iopcon(i:i) != ' ') {
			break
		} else {
			iopcon(i:i) = icnull
		}
	}
	ier = 0
	i=1
	j=1
	ifound = 0
	smfnd = .false.
	repeat {
		while (iopcon(i:i)!='$' && iopcon(i:i)!=icnull &&
		   iopcon(i:i)!=';' && j<=80) {
			work(j:j) = iopcon(i:i)
			i=i+1
			j=j+1
		}
		if (j>80) {
			ier = -1
			goto 1000
		}
		if (iopcon(i:i)==icnull) {
			goto 1000
		}
		if (iopcon(i:i)==';') smfnd = .true.
		if (smfnd) {
			work(j:j) = iopcon(i:i)
			i = i + 1
			j = j + 1
			next
		}
		subcnt = subcnt+1
		ifound = 1
		i=i+1
		call wjfren(i,x,il)
		ix=x
		if (il != comma && il != 0) i=i-1
		if (ix<21) {
			ix = icoman - ix
			if (ix <= 0) ix=ix+20
		}
		if (ix>99 | ix<1) {
			varsub = -2
			return
		}
		read(cmdlun,rec=ix,iostat=ier) work2

		do ii = 1, 79 {
	                # check for comments
			if (work2(ii:ii+1) == bckslpnd) {
			        #comment, remove it.
				if (ii > 1) {
					if (work2(ii-1:ii-1) == bckslpnd(1:1)) {
						# comment was escaped
						break
					}
				}
				do iii = ii, 80 {
					work2(iii:iii) = ' '
				}
				break
			}
		}

		for (ii=80; ii>0; ii=ii-1)
			if (work2(ii:ii)!=' '       &&
			    work2(ii:ii)!=char(10) &&
			    work2(ii:ii)!=icnull) break
		for (jj=1; jj<=ii & j<=80; jj=jj+1) {
			work(j:j) = work2(jj:jj)
			j=j+1
		}
		if (j>=81) {
			ier = -1
			goto 1000
		}
	} until (i>80)
1000    for(i=1; i<=80; i=i+1) {
		if (work(i:i)!=icnull) iopcon(i:i) = work(i:i)
		else iopcon(i:i) = ' '
		work(i:i) = icnull
	}
	if (subcnt>50) ier=-1
	if (ifound==1 && ier==0) goto 10
# old way	if (redire && subcnt>0) write(ttyout,2) iopcon(1:79)
#
# if any $ substitutions were done, print line
#          if ioutverbose (verbose supression) is zero
#
	if ((subcnt>0) & (ioutverbose == 0)) {
			 write(ttyout,2) iopcon(1:80)
	}
	varsub = ier
	return

2       format(1x,a)

	end
