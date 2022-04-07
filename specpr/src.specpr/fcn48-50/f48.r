	subroutine f48(ic)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lblg"
	include "../common/lbl7"
	include "../common/lbl3"
	include "../common/lbl4"
	include "../common/tablet"
	include "../common/lundefs"
	include "../common/alphabet"

	character*80 strg
	integer*4 ztmp1,ztmp2

	ic = ihe
	xlast=-9999.0

3	call hreset(1)
	call whedr2
	write(ttyout,5)
	call crtin
	ich = 1
	call wjfren(ich,x,il)
	if (il == ihe | il == ihx) {
		ic = ihe
		goto 2000
	}
#
# check for tablet, and initialize 
#
1300	write (6,*) '*s32^'
	read (5,1475,iostat=ier) strg
	if (ier!=0) {
		write (6,*) '.'
		goto 1300
	}
	if (strg(1:1)!='1') {
		write (6,*) 'No Tablet Connected!!!'
		return
	} else {
		write (6,*) '*j1A'
	}


#
#  get boundries of plot
#
	call getcor(ichk,xmin,xmax,ymin,ymax,xstp,xlow,ystp,ylow)
		if (ichk==ihe || ichk==ihx) {
			ic=ihx
			go to 2000
		}
#
# set up graphics 
#
191	call respag
	call hpline(1)
	call sb(0)
	diff = ystp-ylow
	call alplty(ylow,ystp)
	xlast=-9999.0
	wmina=xlow
	wmaxa=xstp
	call wvplta(SPMAXCHAN,xstp,xlow,1)
	call serase(0,300,511,368)
	call movabs(0,300)
	call sb(0)
	call texmod
	write (ttyout,59)
	call movabs(0,359)
	call sb(0)
	call texmod
	write (ttyout,58)
#
# suppress cursor display
#
       write(ttyout,*) '*dL'
#
# get points from tablet
# total number of points = maxchn
#
4	call serase(0,318,511,348)
	call movabs(0,338)
	call sb(0)
	call texmod
	write(ttyout,10)
	do i=1,maxchn {
100             ichk=0
		call getpt(ichk,x,y,xmin,ymin)
		if (x > xstp || ichk!=0) {
16			call serase(0,318,511,348)
			call movabs(0,338)
			call sb(0)	
			call texmod
			if (x>xstp) {
				write(ttyout,20)
			}
			write (ttyout,21)
			call crtin
			iw=1
			call wjfren(iw,a,il)
			if (il == ihr) {
				call serase(0,318,511,348)
				call movabs(0,338)
				call sb(0)
				call texmod
				write (ttyout,41)
				go to 100
			}
			if (il == ihb) {
				go to 191
			}
			if (il == ihx) {
				ic = ihx
				goto 2000
			}
			if (il == ihe) {
			     ipts = i-1
			     goto 1000
			}
			go to 16
		}
		call pltdat (xstp,xlow,ylow,diff,y,x,ylast,xlast)
		call serase(0,318,511,348)
		call movabs(0,338)
		call sb(0)
		call texmod
		write(ttyout,60) i,x,y
		dataa(i) = x
		datac(i) = y
	}
#
# get and decode the file to write data into
#
1000	call eralph
	nchans=ipts
	itchan=ipts

#  get title for x axis data
#
	write(ttyout,90)
	call mthwrt(ztmp1,lun,iprt,ztmp2)
	do ii=1,nchans {
		data(ii)=dataa(ii)
	}
	write (ihist,400) ipts
	mhist = ' '
	call titles(ititl1,iopcon,ititl,itl,ier,iprodp,ibncon,xlow,xstp)
	if (ier==ihx || ier==ihe) {
		ic=ier
		write (ttyout,*) '*j0A'
		return
	}
#
# write x axis values
#
	write(ttyout,99) id,iprt
	call wrifil(iprt,lun,ier)
#
# Ok, now plot the data
#
	write (ihist,411) ipts
	write (ttyout,91)
	call mthwrt(ztmp1,lun,iprt,ztmp2)
	call titles(ititl1,iopcon,ititl,itl,ier,iprodp,ibncon,ylow,ystp)
	if (ier==ihx || ier==ihe) {
		ic=ier
		write (ttyout,*) '*j0A'
		return
	}
	itrol(1)=ihd
	itrol(2)=iprt
	itrol(3)=iha
	bbnd=ylow
	ubnd=ystp
	call wriout
#
# write y axis
#
	do ii=1,maxchn {
		data(ii) = datac(ii)
	}
	write(ttyout,78) id ,iprt
	call wrifil(iprt,lun,ier)

2000    call rstart(1)
	write (ttyout,*) '*j0A'
	return
5       format(1x,'Function f22: Tablet Graph',//,5x ,
		'This routine accepts data from a digitizer tablet ',/,5x,
		'and writes the x values and the y values in two ',/,5x,
		'separate files',//,5x,
		'Press return to continue',/)
10	format(20x,'Enter Points')
20      format('',20x,'POINT OUT OF BOUNDS')
21	format( 10x,'type  r  to redo last point,     b  to begin over,',/,
		10x,'      e  to write,           or  x  to exit',/)
41      format(1x,'Reenter the point',/)
58      format(27x,'Digitizing Entry Routine')
59	format(25x,'(press  e  or  x  when done)')
60      format(5x,'Data coordinate ',i3,5x,'x = ',f13.6,3x,'y = ',f13.6)
70      format(///,1x,'Type in the File id where you want the data to go.',/)
78      format(1x,'Writing y axis values to ',a,i4,//)
80      format(1x,'---- incorrect entry -----')
85      format(1x,'---- invalid file protection ----')
90      format(10x,'X-Axis Information',/,79('*'))
91      format(/,10x,'Y-Axis Information',/,79('*'))
98      format(1x,'enter title of the vertical (y) axis',/,1x,39(1h-),'i',/)
99      format(1x,'Writing x axis values to ',a,i4,//)
111     format(1x,'Another graph?',/)
400     format(1x,'f22: x (horiz) axis values from tablet graph ',i3,
			'points')
411     format(1x,'f22: y (vert) axis values from tablet graph ',i3,
		' points')
1475	format(a80)
1499	format('Write Errors!!!')
	end
