	subroutine f38(ic)
	implicit integer*4 (i-n)
#ccc  name:  f38
#ccc  version date:  Sept. 2, 1993
#ccc  author(s):  Roger N. Clark
#ccc  language:  Ratfor
#ccc
#ccc  short description: 
#ccc            compute the Kubelka Munk Remission function
#ccc		
#ccc  algorithm description
#ccc  system requirements:
#ccc  subroutines called:
#ccc
#ccc
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
#     Kubelka Munk Remission function
#
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl3"
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
#RED
	integer*4 iwidok     # function

	character inam*8
	real*4 x
	integer*4 i,iwtmpf,irecw

#

	if (ictrl==-1) {
		ic=ihx
		write(ttyout,55)
		return
	}

        do i=1,maxchn {          # save dataa because wavelengths are put there
                datab(i)=dataa(i)
        }


	call eralph
	call whedr2
	write(ttyout,115) idv1,ifl1,ititl

140	call crtin
	i=1
150	call wjfren (i,a,il)
	if (iwidok(il) == 1) {
		iwtmpf = il
		call wjfren (i,b,ik)
		if (b<=0 || b>maxrec) {
			call what (i)
			write(ttyout,165)
			go to 140
		} else {
			irecw=b
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
			call whedr2
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		go to 600
	} else {
			irecw=itrol(2)
			iwtmpf = itrol(1)
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
	}
#
	do i = 1, nchans {
			x = datab(i)
			if (x == -1.34e34) {
				datac(i) = -1.23e34
			} else if ((abs(x) > 0.1e18) | (abs(x) < 0.1e-35)) {
				write (ttyout,285) i
				datac(i) = -1.23e34
			} else {
				datac(i) = (1-(x*x))/(2.0*x)
			}

	}
		
#
#
#     ************* determine history **********************************
	call namdev (idv1,inam)
	write(ihist,500) inam,ifl1,nchans
#
#
#     ******************** program end *********************************
600     call rstart(1)
	return
55      format (' *** ERROR -- INVALID FILE STATUS ***',/,
'     program will EXIT.',/)

115     format (' Function f38: Kubelka-Munk Remission Function',//,
'     Operating on: ',a,i4,':',a,//,
'     Enter new wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

165     format (' *** ERROR -- INVALID INPUT re-enter ***:',/)

285     format (' *** WARNING *** : inout value too ',
			'small or too large ', /,
'     output channel ',i5,'deleted.',/)

302     format (' Calculations started using previous errors:',/)

305     format (' *** ERROR -- ',
		'INVALID FILE PROTECTION for reading of errors ***',/)

500     format ('f38:',a8,' r',i7,' KM remission, ',i6,' chans')

	end
