subroutine restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,owvmax,olbnd,
		  oubnd,datsc2,nchans)

#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
#ccc
#
# This subroutine called many times RESTABlishes the plot parameters
# to their respective values before the current action was taken
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

  integer*4 chanel,nchans

	include "../common/spmaxes"   # max parameters, must be first

  real*4 wvmin,wvmax
  real*4 ubnd,lbnd,diff
  real*4 error(SPMAXCHAN)
  real*4 owvmin,owvmax
  real*4 oubnd,olbnd
  real*4 datsc2(SPMAXCHAN)
  




# restore error array to its original value 
	do chanel=1,nchans {
		error(chanel)=datsc2(chanel) 
	}
# restablish plot parameters
	wvmin=owvmin
	wvmax=owvmax
	ubnd=oubnd
	lbnd=olbnd
	diff=ubnd-lbnd



return

end
