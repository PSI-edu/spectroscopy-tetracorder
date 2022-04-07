	subroutine dtsort (wav,isort,nchans)
	implicit integer*4 (i-n)
###########################################################

#ccc  version date: 10/02/85
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine makes an array (sort) which
#ccc                   points to the data array values in increasing
#ccc                   wavelengths.
#ccc			10/02/85: changed to just call bubble which
#ccc			does the same thing, only better.
#ccc
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: wav,sort,nchans
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
##########################################################

	include "../common/spmaxes"   # max parameters, must be first

	real*4     wav(SPMAXCHAN)
	integer*4 isort(SPMAXCHAN)

	call bubble (wav,isort,nchans)

	return
	end
