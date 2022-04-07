subroutine sfwhm(xdata,datsc1,bndctr,reflect,chan1,chan2,chnhf1,chnhf2,found2)
#
#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
# This routine finds the appropriate channels where a given reflectance value
# interesects the data representing a band 
# if two intersection points are not found then found2 is returned
# as false 
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
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"

  integer*4 chan1,chan2,chnhf1,chnhf2,chanel
  logical found1,found2
  real*4 bndctr,reflect,xdata(SPMAXCHAN),datsc1(SPMAXCHAN)
 
# Automatically select fwhm 

	  found1=.false.		# have not found first point
	  found2=.false.		# have not found second point
	  chnhf1=0
	  chnhf2=0
	  chanel=chan1
while (.not.(found2) & (chanel<=chan2) ) {
          if (datsc1(chanel)==-1.23e34 | xdata(chanel)==-1.23e34) {
	  } else {		    
          	if (datsc1(chanel)<=reflect & .not.(found1) ) {
	     	  chnhf1=chanel
	          found1=.true.
                 }

	         if (datsc1(chanel)>=reflect & found1 & 
		     xdata(chanel) >=bndctr ) {
	     		 chnhf2=chanel
	      		 found2=.true.
                 }
	   }		# end else point is not a deleted point 
	        chanel=chanel+1

  }		# end while 

	  if (.not.(found2)) {
	     call movabs(0,359*2)
	     call sb(0)
	     write(ttyout,100) reflect
100	     format('fwhm not found, must select manually ',
			'(half max point=', f14.6,')')
	  }

return
end
