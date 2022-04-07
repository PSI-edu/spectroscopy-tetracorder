subroutine gfiles(x,ctwave,bwidth,devctw,devbdw,devres,fnctw,fnbdw,fnres,
		  numbdw,nchans,erflag)

#ccc  name: gfiles
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
# 
#		x     : array of input wavelengths (? RNC 9/23/96)
#		ctwave: array of center wavelengths returned   
#		bwidth: array of corrresponding effective bandwidths returned
#		devctw: device of center wavelength record
#		devbdw: device of bandwidth record
#		devres: device of input resolution record 
#		fnctw : record of center wavelengts
#		fnbdw : record of bandwidthts
#		fnres : record number of input resolutions 
#		numbdw: number of bandwidths
#		erflag: error flag for read and other things
#ccc
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables
#		wavdif: largest difference between ctwave(i) and x(j)
#		wavmin: smallest wavelenght in x (wavlength array)
#		wavmax: largest wavelength in x (wavelength array)
#		conind: index of resol where x(j) is closest to ctwave(i)
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc


implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

include "../common/lundefs"
include "../common/label1"
include "../common/alphabet"



integer*4 tmpctw,tmpbdw,tmpres
integer*4 numctw,numbdw,numres 
integer*4 devctw,devbdw,devres
integer*4 fnctw,fnbdw,fnres
integer*4 erflag,isupres
integer*4 conind
real*4 x(SPMAXCHAN),ctwave(SPMAXCHAN),bwidth(SPMAXCHAN)
real*4 wavmin,wavmax
real*4 wavdif



		erflag=0


20		write(ttyout,21)
21		format(/,
			'Enter file ID and record which contains center',
			' WAVELENGTHS of OUTPUT data set',/,
			'     (file ID must be upper case for wavelength sets)')

				
		call crtin
		i=1
		call wjfren (i,a,il)
		if (il == ihe || il == ihx) {
			erflag=1	
			goto 1000
		}
		if (il==0) {
			call what (i)
			write(ttyout,125)
			go to 20
		} else {
			devctw=il
			itmp = devctw
			call lcasew(itmp)
			call devlun (4,itmp,lun)
			if (lun==0) {
				call what (i)
				write(ttyout,125)
				go to 20
			}
			call wjfren(i,xtemp,il)
			fnctw=xtemp
			call devok(4,itmp,fnctw,lun,ier)


			if (ier!=0) {
				write(ttyout,155)
				erflag=1
				goto 1000
			}else {
				tmpctw=fnctw
				call redfil(tmpctw,lun,ier)
				numctw=itchan
				write (ttyout, 33) ititl,itchan
33				format ('Center wavelengths= ',a,
					'   chanels=',i5,/)
			}

			do j=1,numctw 
				ctwave(j)=data(j)

		}

35		write(ttyout,36)
36		format(//,
			'Enter file ID and record which contains BANDWIDTHS ',
			'of OUTPUT data set',/,
			'followed by an  s  to suppress warning messages about',
                        'small bandwidths')

				
		isupres = 0
		call crtin
		i=1
		call wjfren (i,a,il)
		if (il == ihe || il == ihx) {
			erflag=1
			goto 1000
		}
		if (il==0) {
			call what (i)
			write(ttyout,125)
			go to 35
		} else {
			devbdw=il
			call devlun (4,devbdw,lun)
			if (lun==0) {
				call what (i)
				write(ttyout,125)
				go to 35
			}
			call wjfren(i,xtemp,il)
			if (il == ihs) isupres = 1
			if (il != 0 & il != ihs) {
				call what (i)
				write(ttyout,125)
				go to 35
			}
			fnbdw=xtemp
			call devok(4,devbdw,fnbdw,lun,ier)
	



			if (ier!=0) {
				write(ttyout,155)
				erflag=1
				goto 1000
			}else {
				tmpbdw=fnbdw
				call redfil(tmpbdw,lun,ier)
				numbdw=itchan
				write (ttyout,39) ititl,itchan
39				format ('Output Bandwidths= ',a,
					'    channels=',i5,/)
			}

			call wjfren(i,xtemp,il)
			if (il == ihs) isupres = 1
		}

#	Error trap: number of center wavelengts and bandwidths differ

		if (numbdw!=numctw) {
			write (ttyout,41)
41			format (//,'Number of bandwidths does not equal', 
				' number of  center wavelengths',/,
				'exiting')	
			erflag=1
			goto 1000
		}




		do j=1,numbdw  {
			bwidth(j)=data(j)
		}


65		write(ttyout,66)
66		format(//,
			'Enter file ID and record which contains BANDWIDTHS of',
			' INPUT spectrum ',/,
			' This is the resolution spectrum of the ',
			'current wavelength set.',/)

				
		call crtin
		i=1
		call wjfren (i,a,il)
		if (il == ihe || il == ihx) {
			erflag=1
			goto 1000
		}
		if (il==0) {
			call what (i)
			write(ttyout,125)
			go to 65
		} else {
			devres=il
			call devlun (4,devres,lun)
			if (lun==0) {
				call what (i)
				write(ttyout,125)
				go to 65
			}
			call wjfren(i,xtemp,il)
			fnres=xtemp
			call devok(4,devres,fnres,lun,ier)
	
			if (ier!=0) {
				write(ttyout,155)
				erflag=1
				goto 1000
			}else {
				tmpres=fnres
				call redfil(tmpres,lun,ier)
				numres=itchan
				write (ttyout,69) ititl,numres
69				format ('Input Bandwidths= ',a,
					'     channels=',i5,/)
			}
#	Resolutions are now in array data (accomplished by redfil)
		}

#  Check to see if the # of channels in the input wavelength 
#  record and the # of channels in the resolution record are equal


if ( numres!=nchans )  {
	write(ttyout,70)
70	format(//,
		' Resolution set does not correspond to wavelength',
		'set ',/,'exiting')
	goto 1000
}




# Find minimum and maximum wavelengths of input spectrum x
# just in case something strange is going on

wavmin=x(1)
wavmax=x(1)
do i=2,nchans {
	if ( x(i) < wavmin )
		wavmin=x(i)
	if ( x(i) > wavmax )
		wavmax=x(i)
}


# Calculation of effective bandwidths


i=1
while (i<=numbdw) {
	if ( (ctwave(i)>wavmin) & (ctwave(i)<wavmax) ) {
		wavdif=1.0e34
		do j=1,numres {
			tmpdif=abs( x(j) - ctwave(i) )
			if (tmpdif < wavdif) {
				wavdif=tmpdif
				conind=j
			}
		}
		if ( bwidth(i) <= data(conind) ) {
			if (isupres == 0) write(ttyout,71)conind
71			format('NOTE: Output bandwidth is smaller',
			       ' than input bandwidth channel',i5,
			       ' of input')
			bwidth(i)=data(conind)*0.01
			ctwave(i)=x(conind)
		} else  {
			bwidth(i)=sqrt( (bwidth(i))**2 - (data(conind))**2)
		}
	} else  {
		write (ttyout,73) i
73		format ( 'Convolution center wavelength is not within ',/,
		       'the wavelength range of the input spectrum ',
			'(channel=',i5,' ',/,
		       'throwing convolution about this wavelength out')
		bwidth(i)=-1.23e34
	}
	i=i+1
}			


125 format( ' ***--ERROR-- invalid input to program.  re-enter: ',/)

155 format( ' ***--ERROR-- invalid file protection   ***',/)



1000  if (erflag==1) {
		write(ttyout,1010)
1010		format('ERROR, exiting gfiles')	
	}

return 
end
