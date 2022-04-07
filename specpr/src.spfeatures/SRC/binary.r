	subroutine binary
	implicit integer*4 (i-q)
	include "spfeat.h"
	include "../../src.specpr/common/spmaxes"
	include "../../src.specpr/common/spfeat"
	
	integer*4 last,vlast,zlun,zlun2,ijunk(32),fsize,zlast
	equivalence (sptitl,ijunk)
	zlun=1
	zlun2=0
	ttyout=6
         
#
# Open file
#
	open (zlun,file=filnam,status='old',iostat=ier,access='direct',
		form='unformatted',recl=128)

#
# Check for output errors
#
	if (ier!=0) {
	 	write (ttyout,*) 'Problems opening data file, error=',ier,'  quitting'
		write (ttyout,*) 'file: ', filnam
		stop
	}


#
# Open output file if necessary
#
	if (oflag==1) {
		call spopen(ofile,zlast,ier)
# check for i/o errors
		if (ier==-3) {
			write (ttyout,201) ofile
			stop
		}
		if (ier==-4) {
			write (ttyout,202) ofile
			stop
		}
		if (ier==-5) {
			write (ttyout,203) ofile
			stop
		}
		if (ier!=0) go to 99
	}
		
#
# Start reading at last=1
#
	ilen=int(fsize(filnam//char(0)))
	vlast=int(float(ilen)/128.0)

	do last=1,vlast {
		ier=0
		read (zlun,rec=last,iostat=ier) ijunk
#
# check for i/o errors
		if (ier!=0) go to 99
#
# look for matches
#
		if (center>=cenmin & center<=cenmax) {
		if (width>=widmin & width<=widmax) {
		if (depth>=depmin & depth<=depmax) {
#RED
#		if (errd>=errmin & error<=errmax) {
		if (errd>=errmin & errd<=errmax) {
		if (asym>=asymin & asym<=asymax) {
			if (oflag==1) {
				write (zlun2,rec=zlast+1,iostat=ier) ijunk
				zlast=zlast+1
			} else {
				if (verb==1) {
				     write (ttyout,110) sptitl,center,width,
					depth,errd,asym,contin,iter,xfract,
					icomnt,fname,irecn
				} else {
				     write (ttyout,100) sptitl,center,width,
					depth,errd,asym,contin,iter,xfract,
					fname,irecn
				}
			}
		}
		}
		}
		}
		}
	}
99	if (ier!=0) {	
		write (ttyout,*) 'I/O problems, quitting'
		write (ttyout,*) 'error = ', ier
		stop
	}
100	format(a17,1x,f8.4,1x,f6.4,2(1x,f5.3),1x,f5.2,1x,f4.2,1x,f3.0,1x,f4.2,1x,a8,i6)
110	format(a40,1x,f8.4,1x,f6.4,2(1x,f5.3),1x,f6.3,1x,f4.2,1x,f3.0,1x,f5.3,
		1x,a20,1x,a8,i6)
201 	format(' Error in reading file name: ',a,' try again ',/)
202 	format(' Cannot have imbedded blanks in file name: ',a,' try again',/)
203	format(a,' exists, and is not compatable with this'/,
		'type of output, try again',/)
	return
	end
