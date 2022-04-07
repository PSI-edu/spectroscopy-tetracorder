	subroutine todms (itime,scale,id,im,sec)
	implicit integer*4 (i-n)

#ccc  name: todms
#ccc  version date: 5/3/85
#ccc  author(s): Rob Burtxlaff and Roger Clark
#ccc  language: Ratfor
#ccc
#ccc  short description:
#ccc                    This subroutine converts time in scaled seconds into
#ccc                    degrees, minutes, seconds.
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description:
#ccc  parameter description: none
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information: none
#ccc  NOTES:
#ccc

	real*4 sec
	integer*4 scale

	jtime = itime/scale
	id = int(jtime/3600)
	nh = mod (jtime,3600) 
	im = int(nh/60)
	isec = mod(jtime,60)
	itemp = itime - int(scale)*(3600*id + 60*im +isec)
	sec = real(isec) + real(itemp)/real(scale)
	return   
	end
