	subroutine frdms ( itime,scale,id,im,sec,isgn)
	implicit integer*4 (i-n)

#ccc  name: frdms
#ccc  version date: 5/3/85
#ccc  author(s): Rob Burtzlaff and Roger Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine converts degrees, minutes, seconds into number
#ccc         of seconds past beginning of day. 
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description: itime: result; scale: integer scaling factor
#ccc				 id= degrees (positive integer)
#ccc				 im= minutes       "
#ccc				 sec=real seconds (positive)
#ccc				 isgn= integer sign: positive =1, negative =-1
#ccc  parameter description:
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information: none
#ccc  NOTES:
#ccc


	real*4 sec
	integer*4 id, im, itime, scale

	itime = idint((dble(3600*id + 60*im) + dble(sec)) * dble(scale))
	if (isgn == -1) itime = -1 * itime

	return
	end
