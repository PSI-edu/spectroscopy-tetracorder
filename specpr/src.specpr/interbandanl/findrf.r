function findrf(xdata,datsc1,chanlf,chanrt,xpoint)

#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description: this function determines the reflectance of
#     point given a horizontal position and a data set by a linear 
#     interpolation 
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
	
	integer*4 chanel,chanlf,chanrt,chnavg
	real*4 xpoint,mavg,bavg
        real*4 xdata(SPMAXCHAN), datsc1(SPMAXCHAN)
	character*80 outline

	chanel=chanlf
        chnavg=chanlf-1 
	while (xpoint>xdata(chanel) & chanel<chanrt ) {
	        chnavg=chanel	
		chanel=chanel+1
	}
# find first set of points which are not deleted  
	while((datsc1(chanel)==-1.23e34 | datsc1(chnavg)==-1.23e34) & chanel<
  	   chanrt & chnavg>chanlf) {
		if (datsc1(chnavg)==-1.23e34 ) {
		  chnavg=chnavg-1
		}
		if (datsc1(chanel)==-1.23e34 ) {
			chanel=chanel+1
		}
	  }	
	if (chanel==chanrt)  {
	  if (datsc1(chnavg)==-1.23e34) {
		call movabs(0,0) 
		call sb(0)
		pause '?????????'
		write(outline,100) char(0)
		call gwrite(outline)
100		format('Data is too bad, program will crash ',a1)
# dummy value strange and unknown results will follow 
		findrf=datsc1(chanrt)/2.0
		return
	  }	
	}
	mavg=(datsc1(chanel)-datsc1(chnavg))/(xdata(chanel)-
		xdata(chnavg))
	bavg=datsc1(chanel)-mavg*xdata(chanel)
	refavg=mavg*xpoint+bavg

	findrf=refavg
return
end
