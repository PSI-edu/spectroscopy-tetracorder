integer*4 function mindat(data,lower,upper)


#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
#ccc
#
# This function returns the index of the minimum data value 
# within the array elements data(lower) to data(upper)
# the data array is assumed to be real 
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

   integer*4 lower,upper,smchan,chanel
   real*4 small
   real*4 data(SPMAXCHAN)
   smchan=lower
   small=data(lower) 
   do chanel=lower+1, upper {
     if (data(chanel)!=-1.23e34) {
     	if (data(chanel)<small) {
       		small=data(chanel)
       		smchan=chanel
      	}
      }
   }

   mindat=smchan

  end

