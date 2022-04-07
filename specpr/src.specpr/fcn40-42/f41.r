	subroutine f41(ic)
	implicit integer*4 (i-n)

#ccc  name: f41
#ccc  version date: 8/12/86
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
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
#     block binning routine
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
	integer*4 iwidok   # function

	character*8 inam, inamw
	integer*4 ifilw, inpchn, iflag


#
#
#*********************************************************************
#this program takes an input file from specpr and                    * 
# outputs the number of occurances in a given bin                    *
# bin size is defined by the user.
#*********************************************************************


	if (ictrl==-1) {
		ic=ihx
		write(ttyout,55)
		return
	}

	do i=1,maxchn {
		data(i)=0.0
		datab(i)=dataa(i)
		datac(i)=0.0
		error(i)=0.0
	}


	call eralph
	call whedr2
	write(ttyout,115) idv1,ifl1,ititl
	inpchn = nchans   # number of input channels

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
			itrol(1) = iwtmpf
			itrol(2) = irecw
			ifilw = irecw
			call namdwv (iiwtmpf, inamw)
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		go to 600
	}

#     ************************* start computation **********************

	iflag = 1  # write diagnostic error messages
	call xbin (inpchn, dataa, dataw, nchans, datac, iflag)
	if (iflag == -1) {
		ic = ihx   #fatal error
		return
	}

#
#
#     ************* determine history **********************************
	call namdev (idv1,inam)
	write(ihist,500) inam,ifl1,inpchn, inamw, irecw
#
#
#     ******************** program end *********************************
600     call rstart(1)
	return
55      format ( ' *** error -- invalid file status ***',/,
'     program will exit.',/)

115     format (' Function f41: Block Binning Routine',//,
'     This function finds the number of occurances in a given block.', //,
'     Operating on: ',a,i6,':',a,//,
'     A block is defined as the width starting at a given value and is',/,
'     defined by the output wavelength set.  The bin width is computed',/,
'     from the mid-points between channels.  The output data are the', /,
'     number of values that occur within a given bin.',//,
'     Enter the file ID and record number of the output wavlength set',//,
' UNDER DEVELOPMENT: DO NOT PROCEED!',//,
'     or "e", or "x" to exit:',/)

165     format (' *** error -- invalid input re-enter ***:',/)

500     format ('f41:',a8,' r',i6,'(ch=',i6,') bin to wave:',
		a, ' r',i6)

	end
