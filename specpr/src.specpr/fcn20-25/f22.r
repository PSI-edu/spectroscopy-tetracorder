	subroutine f22(ic)
	integer*4 ic

	print *,"this routine is not yet avaliable"

	return
#	subroutine f22(ic)
#	implicit integer*2 (i-n)
#
#	include ../common/blank
#	include ../common/label1
#	include ../common/lblg
#	include ../common/lbl7
#	include ../common/lbl3
#	include ../common/lbl4
#	include ../common/tablet
#	include ../common/lundefs
#	include ../common/alphabet
#
#
#
#	ic = ihe
##
##  open and allocate tablet
##
#3       call tabalc(ifd)
##
#	call hreset(1)
#	call whedr2
#	write(ttyout,5)
#	call crtin
#	ich = 1
#	call wjfren(ich,x,il)
#	if (il == ihe | il == ihx) {
#		ic = ihe
#		iif = iclose(ifd)
#		goto 2000
#	}
##
##
##  get boundries of plot
##
#	call getcor(ifd,xmin,xmax,ymin,ymax,xstp)
##
## get points from tablet
##
## total number of points = maxchn
##
#4       write(ttyout,10)
#	for(i=1;i<=maxchn;i=i+1) {
#100             call getpt(ifd,x,y,xmin,ymin)
#		if (x > xstp) {
#			write(ttyout,20)
#			call crtin
#			ich = 1
#			call wjfren(ich,x,il)
#			if (il == ihb) {
#				goto 4
#			}
#			if (il == ihx | il == ihe) {
#				ic = ihx
#				iif = iclose(ifd)
#				goto 2000
#			}
#			if (il == ihw) {
#			     ipts = i-1
#			     goto 1000
#			}
#			write(ttyout,41)
#			goto 100
#		}
#		write(ttyout,60) i,x,y
#		data(i) = x
#		datac(i) = y
#	}
##
## get and decode the file to write data into
##
#1000    iif = iclose(ifd)
#	write(ttyout,70)
#	ich = 1
#	call crtin
#	call wjfren(ich,x,il)
#	if (x != 0 | ich >= 80) {
#		write(ttyout,80)
#		goto 1000
#	}
#	id = il
#	call devlun(4,id,lun)
#	call devsta(lun,ista,ier,iprt)
#	if (ier !=0 | iprt == -1) {
#		write(ttyout,85)
#		goto 1000
#	}
##
##  get title for x axis data
##
#	write(ttyout,90)
#	ich = 1
#	call crtin
#	ititl = iopcon
#
##
##history for x (horizontal values)
##
#	write (ihist,400) ipts
#	mhist = ' '
##
## write x axis values
##
#	iprt = iprt+1
#	write(ttyout,99) id,iprt
#	call wrifil(iprt,lun,ier)
##
## get title of y (vertical) axis
##
#	write(ttyout,98)
#	ich = 1
#	call crtin
#	ititl = iopcon
##
##  history for y axis values
##
#	write (ihist,411) ipts
##
## write y axis
##
#	iprt = iprt + 1
#	for(ii=1;ii<= maxchn;ii=ii+1) data(ii) = datac(ii)
#	write(ttyout,78) id ,iprt
#	call wrifil(iprt,lun,ier)
##
##
#	write(ttyout,111)
#	call crtin
#	ich = 1
#	call wjfren(ich,x,il)
#	if (il == ihy) goto 3
#2000    call rstart(1)
#	return
#5       format(1x,'Function f22: Tablet Graph',//,5x ,
#		'This routine accepts data from a digitizer tablet ',/,5x,
#		'and writes the x values and the y values in two ',/,5x,
#		'separate files',//,5x,
#		'Press return to continue',/)
#10      format(///,1x,'Enter Points From Tablet',/)
#20      format(1x,'POINT OUT OF BOUNDS',/,1x,
#		'type r to redo the point, b to begin over ',/,1x,
#		'or w to write, or e or x to exit',/)
#41      format(1x,'Reenter the point',/)
#60      format(1x,i3,'.  x = ',f15.6,'  y = ',f15.6)
#70      format(///,1x,'Type in the File id where you want the data to go.',/)
#78      format(1x,'Writing y axis values to ',a,i4,//)
#80      format(1x,'---- incorrect entry -----')
#85      format(1x,'---- invalid file protection ----')
#90      format(1x,'enter title of the horizontal (x) axis',/,1x,39(1h-),'i',/)
#98      format(1x,'enter title of the vertical (y) axis',/,1x,39(1h-),'i',/)
#99      format(1x,'Writing x axis values to ',a,i4,//)
#111     format(1x,'Another graph?',/)
#400     format(1x,'f22: x (horizontal) axis values from tablet graph ',i3,
#			' points   ')
#411     format(1x,'f22: y (vertical) axis values from tablet graph ',i3,
#		' points   ')
#	end
	end
