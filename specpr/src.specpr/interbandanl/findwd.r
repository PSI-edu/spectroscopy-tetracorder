subroutine findwd(xdata,datsc1,oxctr,refold,refbnd,nxctr,xwdth1,xwdth2,refnew,
	  	  ochan1,ochan2,nchan1,nchan2,xmin,xmax,lbnd,diff,found)

#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description: This routine finds a full width given data
#     about the past full width and continuum 
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#			xdata: wavelength data
#			datsc1: reflectance data
#			oxcter: the center of the past full width
#			refold: the reflectance of the last full width
#			refbnd: the reflectance of the band at oxcter
#		  	nxctr: new full width x center
#			xwdth1: x coordinate of first new full width		#			xwdth2: x coordinate of second new full width	
#			refnew: new full width reflectance
#			ochan1: past channel of first full width point
#			ochan2: past channel of second full width point
#			nchan1: new channel of first full width point
#			nchan2: new channel of second full width point
#			xmin: minimum coordinate of x axis 
#			xmax: maximum coordinate of x axis
# 			lbnd: minimum corrdinate of y axis
#			diff: height of y axis

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

#  RED
integer*4 gxwth1,gywth1,gxwth2

integer*4 gxctr,gyctr,bnchn1,bnchn2
integer*4 ochan1,ochan2
integer*4 nchan1,nchan2
integer*4 chanel,itmp
logical found
real*4 oxctr,refold,refbnd
real*4 nxctr,refnew 
real*4 xwdth1,xwdth2
real*4 xdata(SPMAXCHAN),datsc1(SPMAXCHAN)
real*4 xmin,xmax,lbnd,diff


real*4 mwdth1,mwdth2
real*4 bwdth1,bwdth2

character*80 outline # xwindows writes



# find new reflectance level of next ? width maximum
		refnew=(refold+refbnd)/2.0

# Automatically select fwhm 
		  call sfwhm(xdata,datsc1,oxctr,refnew,ochan1,ochan2,nchan1, 
			     nchan2,found)
		  if (found) {

# find the xcoordinates of the points of intersection between 
# the horizontal line at the new band width (refnew) and the data 
# first find two lines in the form y=mx+b
		  bnchn1=nchan1-1
		  while ((datsc1(bnchn1)==-1.23e34 | 
			xdata(bnchn1)==-1.23e34) & bnchn1>ochan1) {
				bnchn1=bnchn1-1
		  }
		  mwdth1=(datsc1(bnchn1)-datsc1(nchan1))/(xdata(bnchn1)-
			xdata(nchan1))
                  bwdth1=datsc1(nchan1)-mwdth1*xdata(nchan1) 
		
		  bnchn2=nchan2-1
		  
		  while ((datsc1(bnchn2)==-1.23e34 | 
			xdata(bnchn2)==-1.23e34) & bnchn2>ochan1) {
				bnchn2=bnchn2-1
		  }

	  	  mwdth2=(datsc1(bnchn2)-datsc1(nchan2))/(xdata(bnchn2)-
			xdata(nchan2))
                  bwdth2=datsc1(nchan2)-mwdth2*xdata(nchan2)
		
		xwdth1=(refnew-bwdth1)/mwdth1                  
		xwdth2=(refnew-bwdth2)/mwdth2

# convert coordinates of line at wdth 
		call gconvt(gxwth1,gywth1,xwdth1,refnew,lbnd,diff,
			   xmin,xmax)
		call gconvt(gxwth2,gywth1,xwdth2,refnew,lbnd,diff,
			    xmin,xmax)

# determine the center of the intersection points and the
# reflectance value where it intersects the data curve
		
		nxctr=(xwdth1+xwdth2)/2.0
	        nchan1=nchan1-1

# draw mark at wavelenght center and centered about refnew 
		
		call gconvt(gxctr,gyctr,nxctr,refnew,lbnd,diff,
			   xmin,xmax)
		itmp=gyctr-5
		call movabs(gxctr,itmp)
		itmp=gyctr+5
		call drwabs(gxctr,itmp)
# draw horizontal mark 
		itmp=gxctr-5
		call movabs(itmp,gyctr)
		itmp=gxctr+5
		call drwabs(itmp,gyctr)
# determine the center of the intersection points and the
		} else  {
		        call movabs(0,349)
			call sb(0)
			write(outline,500) char(0)
			call gwrite(outline)
500			format('cannot select another width data is ', 
			       ' too noisy',a1)
		}
return
end

