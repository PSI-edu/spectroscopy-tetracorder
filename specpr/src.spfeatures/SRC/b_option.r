	subroutine bopt
	implicit integer*4 (i-n)
	include "spfeat.h"
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/lbl4"
	character*132 linstr
	integer*4 widbeg,widend,depbeg,depend
	integer*4 errbeg,errend,cenbeg,cenend,asybeg,asyend
	integer*4 colpos  
	real*4 center,depth,width,error,asym

	widbeg=40
	widend=47
	depbeg=48
	depend=54
	errbeg=31
	errend=39
	cenbeg=22
	cenend=30
	asybeg=55
	asyend=61

# 
# Open data file
#
	open(unit=10,file=filnam,iostat=ier,status='old',
		access='sequential',form='formatted')
  
	if (ier!=0) {
		write(ttyout,800) 
		stop
	}
800	format(' unable to open data file, giving up')


#
# read in data
#
1	read(unit=10,fmt=1000,end=9000,err=5000)linstr
	iopcon=linstr
#
#	check center for errors in file
#
	colpos=cenbeg
	call wjfren(colpos,center,il)
	if ( il!=0 | colpos > cenend ) {
	    print *,' error in reading center, giving up  '
	    write(ttyout,'("val of il",a)') il
	    write(ttyout,'(a)') linstr(1:lnb(linstr))
	    stop
	}
#
#	check width for errors in file
#
	colpos=widbeg
	call wjfren(colpos,width,il)
	if ( il!=0 | colpos > widend ) {
	    print *,' error in reading width, giving up  '
	    write(ttyout,'("val of il",a)') il
	    write(ttyout,'(a)') linstr(1:lnb(linstr))
	    stop
	}
#
#	check depth for errors in file
#
	colpos=depbeg
	call wjfren(colpos,depth,il)
	if ( il!=0 | colpos > depend ) {
	    print *,' error in reading depth, giving up  '
	    write(ttyout,'("val of il",a)') il
	    write(ttyout,'(a)') linstr(1:lnb(linstr))
	    stop
	}
#
#	check errors for errors in file
#
	colpos=errbeg
	call wjfren(colpos,error,il)
	if ( il!=0 | colpos > errend ) {
	    print *,' error in reading errors, giving up  '
	    write(ttyout,'("val of il",a)') il
	    write(ttyout,'(a)') linstr(1:lnb(linstr))
	    stop
	}
#
#	check asymmetry for errors in file
#
	colpos=asybeg
	call wjfren(colpos,asym,il)
	if ( il!=0 | colpos > asyend ) {
	    print *,' error in reading asymmetry, giving up  '
	    write(ttyout,'("val of il",a)') il
	    write(ttyout,'(a)') linstr(1:lnb(linstr))
	    stop
	}

#
#
# everthings ok, so see if it fits the parameters
#
#
	if (center>=cenmin & center<=cenmax) {
	  if (width>=widmin & width<=widmax) {
	    if (depth>=depmin & depth<=depmax) {
	      if (error>=errmin & error<=errmax) {
		if (asym>=asymin & asym<=asymax) {
		  write (ttyout,'(a)') linstr(1:lnb(linstr))
		}
	      }
	    }
          }
        }
	go to 1

1000	format(a)
5000	print *,' error reading data file, exiting '
	stop
9000	return
	end
