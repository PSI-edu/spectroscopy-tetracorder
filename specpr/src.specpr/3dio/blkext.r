#*******************************************************************
#   TITLE:        BLOCK EXTRACTION ROUTINE                         *
#                                                                  *
#   PROGRAMMER:   Barry J. Middlebrook                             *
#                 USGS Branch of Geophysics                        *
#                 Denver West Offices                              *
#                 (303) 236-1411                                   *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               As part of the 3d i/o package for extracting       *
#               linear arrays from data cubes this routine gets    *
#               handed the upper left coordinates of the block     *
#               to be averaged and the number of columns and rows  *
#               in that block.  It returns the averaged block in   *
#               the form of a single linear array.  The standard   *
#               deviation for each channel is loaded into the      *
#               array in specpr normally used for error bars.      *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     *
#              datsc3	- array accumulating the sum of xi (x sub  *
#                       i)                                         *
#              datsc4	- array accumulating the sum of xi*xi      *
#              error	- array accumulating the number of data    *
#                       points included in the sums at first, then *
#                       the standard deviation of the mean on exit *
#              data	- array containing the extracted spectra   *
#                       at first, then the average spectra on exit *
#              orgniz	- either bil, bip or bsq (1,2 or 3 resp.)  *
#              xdim	- x dimension of data cube                 *
#              ydim	- y dimension of data cube                 *
#              zdim	- z dimension of data cube                 *
#              lim	- length of arrays containing data & errors*
#              xlim	- size of extracted block in x dimension   *
#              ylim	- size of extracted block in y dimension   *
#              zlim	- size of extracted block in z dimension   *
#              test	- variable to check for divide by zero     *
#                                                                  *
#------------------------------------------------------------------*

	subroutine blkext (chbuff,i2buff,i4buff,r4buff,ioerr,x,y,z,flag,in,n,a)
	# SCCS ID: %Z% %W% %G%

#  Set variable type and include common data blocks
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4      ioerr,flag,x,y,z,orgniz,n,a,in,xdim,ydim,zdim
	integer*4      i4buff(384)
	integer*2      i2buff(768)
	real*4         r4buff(384)
	character      chbuff*1536
	include		"../common/dscrch"
	include		"../common/label1"
	include		"../common/lbl3"
	include		"../common/ioftyp"
	include		"../common/lundefs"

#  Initialize working arrays
	do i=1,4864
	{
	   datsc3(i)=0.0
	   datsc4(i)=0.0
	   error(i)=0.0
	   data(i)=-1.23e34
	}

#  Get the extraction direction and set variable flag
	orgniz=filtyp(10,ftptr(in))
	xdim=filtyp(7,ftptr(in))
	ydim=filtyp(6,ftptr(in))
	zdim=filtyp(8,ftptr(in))

#  Select data format and loop through block of pixels
	if (x==-1)
	{
	   lim=xdim
	   ylim=y+fiobox(1)
	   zlim=z+fiobox(3)

	   do i=y,ylim
	   {
	      do j=z,zlim
	      {
	         if (orgniz == 1)
	         {
	            call redbil (chbuff,i2buff,i4buff,r4buff,ioerr,x,i,j,
	                         flag,in,n,a)
	         }
	         else if (orgniz == 2)
	         {
	            call redbip (chbuff,i2buff,i4buff,r4buff,ioerr,x,i,j,
	                         flag,in,n,a)
	         }
	         else if (orgniz == 3)
	         {
	            call redbsq (chbuff,i2buff,i4buff,r4buff,ioerr,x,i,j,
	                         flag,in,n,a)
	         }
	         else
	         {
	            write (6,*)'ERROR: Data format not specified'
	            return
	         }

	         do k=1,xdim
	         {
	            if (data(k) != -1.23e34)
	            {
	               error(k)=error(k)+1
	               datsc3(k)=datsc3(k)+data(k)
	               datsc4(k)=datsc4(k)+data(k)**2
	            }
	         }
	      }
 	   }
	}
	else if (y==-1)
	{
	   xlim=x+fiobox(2)
	   lim=ydim
	   zlim=z+fiobox(3)

	   do i=x,xlim
	   {
	      do j=z,zlim
	      {
	         if (orgniz == 1)
	         {
	            call redbil (chbuff,i2buff,i4buff,r4buff,ioerr,i,y,j,
	                         flag,in,n,a)
	         }
	         else if (orgniz == 2)
	         {
	            call redbip (chbuff,i2buff,i4buff,r4buff,ioerr,i,y,j,
	                         flag,in,n,a)
	         }
	         else if (orgniz == 3)
	         {
	            call redbsq (chbuff,i2buff,i4buff,r4buff,ioerr,i,y,j,
	                         flag,in,n,a)
	         }
	         else
	         {
	            write (6,*)'ERROR: Data format not specified.'
	            return
	         }

	         do k=1,ydim
	         {
	            if (data(k) != -1.23e34)
	            {
	               error(k)=error(k)+1
	               datsc3(k)=datsc3(k)+data(k)
	               datsc4(k)=datsc4(k)+data(k)**2
	            }
	         }
	      }
 	   }
	}
	else if (z==-1)
	{
	   xlim=x+fiobox(2)
	   ylim=y+fiobox(1)
	   lim=zdim

	   do i=x,xlim
	   {
	      do j=y,ylim
	      {
	         if (orgniz == 1)
	         {
	            call redbil (chbuff,i2buff,i4buff,r4buff,ioerr,i,j,z,
	                         flag,in,n,a)
	         }
	         else if (orgniz == 2)
	         {
	            call redbip (chbuff,i2buff,i4buff,r4buff,ioerr,i,j,z,
	                         flag,in,n,a)
	         }
	         else if (orgniz == 3)
	         {
	            call redbsq (chbuff,i2buff,i4buff,r4buff,ioerr,i,j,z,
	                         flag,in,n,a)
	         }
	         else
	         {
	           write (6,*)'ERROR: Data format not set.'
	           return
	         }

	         do k=1,zdim
	         {
	            if (data(k) != -1.23e34)
	            {
	               error(k)=error(k)+1.0
	               datsc3(k)=datsc3(k)+data(k)
	               datsc4(k)=datsc4(k)+data(k)**2
	            }
	         }
	      }
 	   }
	}
	else
	{
	   write (6,*)'ERROR:  Flag to specify extraction direction not set.'
	   return
	}

#  Calculate statistics
	do k=1,lim
	{
	   # Test for divide by zero
	   test = error(k)*(error(k)-1.0)

#		if (k>159 & k<163) { #DEBUG
#			write (6,*)' chan',k,' n=',error(k),' test=',test
#		}

	   if (test <= 0.1e-4) {
	   # error array should be integer,
           # so if small, it is zero.
	   # Set to zero
		error(k) = 0.0
#		if (k>159 & k<163) { #DEBUG
#			write (6,*)' error set to zero'
#		}
	   }
	   else
	   { # Otherwise calculate:
	      #  Mean
	         data(k)=datsc3(k)/error(k)

	      #  Standard deviation
	         datsc3(k)=-2.*data(k)*datsc3(k)+error(k)*data(k)**2
	         datsc3(k)=datsc4(k)+datsc3(k)
		 if (datsc3(k) < 0.1e-35) {
			error(k) = 0.0
		 } else {
			error(k)=sqrt(datsc3(k)/test)
		 }
#		if (k>159 & k<163) { #DEBUG
#			write (6,*)'           error=',error(k),' datsc3=',datsc3(k)
#		}
	   }
	}

#  End subroutine and return to calling program
	return
	end
