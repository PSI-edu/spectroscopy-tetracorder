	subroutine ticks(from,to,label,lstrt,lincr,tincr)
	implicit integer*4 (i-n)
#ccc  name:
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine calculates the tick marks for a
#ccc                   graph using INICE twice. Once for the labels and
#ccc                   second for the intermediate ticks. Also significant
#ccc                   numbers are determined and the label values are put
#ccc                   into an array.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    inice
#ccc  argument list description:
#ccc    arguments: from,to,label,lstart,lincr,tincr
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
	real*4 label(6),lstrt,lincr

#############################
# call inice for labels
#############################

	call inice (from,to,lincr,6.0,lstrt)

	label(1) = lstrt
	for (i=2;i<=6;i=i+1) {
		label(i) = lstrt + (i-1)*lincr
		if (label(i) > to) {
			for(ii=i;ii<=6;ii=ii+1) label(ii) = -1.23e34
			break
		}
	}

############################
# call inice for small ticks
############################
	call inice (lstrt,lstrt+lincr,tincr,6.0,tstrt)

	return
	end
