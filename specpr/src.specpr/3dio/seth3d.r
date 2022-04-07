	
	subroutine seth3d (in,flag,n,a)   # SCCS ID: %Z% %W% %G%

#*******************************************************************
#   TITLE:            SET HEADER INFO FOR 3D FILE                  *
#   PROGRAMMER:       Barry J. Middlebrook                         *
#------------------------------------------------------------------*
#   DESCRIPTION                                                    *
#              This a subroutine for specpr for the purpose of     *
#              initializing parameters needed for plotting of data *
#              extracted from a 3d file.  Many of the values need  *
#              to be set but at the present do not apply for our   *
#              purposes.                                           *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     *
#             titl3d	- title input by user                      *
#             ititl	- char str containing the title of the     *
#             x,y,z	- either the coordinate of the pixel to    *
#                       extract or the upper left coordinate of    *
#                       the block to extract                       *
#             x,y,zdel	- delta x/y/z or the dimensions in pixels  *
#                       minus 1 of the block to extract            *
#             test	- used to test for single pixel or block   *
#                       extraction                                 *
#             len#	- length of char strs returned by num2ch   *
#             chndx#	- index in char str ititl                  *
#             chnum#	- char strs containing interpreted numbers *
#                                                                  *
#   NOTE: See include files for further documentation on variables *
#                                                                  *
#*******************************************************************

#  Set variable type
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4      n,a,x,y,z,flag,in,test,xdel,ydel,zdel
	integer*4      len1,len2,len3,len4,chndx1,chndx2
	character*10   chnum1,chnum2,chnum3,chnum4
	include        "../common/label1"
	include        "../common/ioftyp"
	integer*2      ibit

#  Set bit flags
	ibit = 0
	call clrbit(icflag,ibit)
	ibit = 1
	call clrbit(icflag,ibit)
	ibit = 3
	call setbit(icflag,ibit)

# Set title
	ititl(1:24) = titl3d(ftptr(in))

#  Put the pixel coordinates into the title
	x=filreq(1)
	xdel=fiobox(1)
	y=filreq(2)
	ydel=fiobox(2)
	z=filreq(3)
	zdel=fiobox(3)
	test=xdel+ydel+zdel

	if (test != -1)
	{
	   if      (flag == 1)
	   {
	      call num2ch (y,len1,chnum1)
	      call num2ch (ydel,len2,chnum2)
	      call num2ch (z,len3,chnum3)
	      call num2ch (zdel,len4,chnum4)
	      chndx1=17
	      chndx2=chndx1+5
	      ititl(chndx1:chndx2)=' px(*,'
	      chndx1=chndx2+1
	      chndx2=chndx2+len1+1
	      ititl(chndx1:chndx2)=chnum1(1:len1)//'+'
	      chndx1=chndx2+1
	      chndx2=chndx2+len2+1
	      ititl(chndx1:chndx2)=chnum2(1:len2)//','
	      chndx1=chndx2+1
	      chndx2=chndx2+len3+1
	      ititl(chndx1:chndx2)=chnum3(1:len3)//'+'
	      chndx1=chndx2+1
	      chndx2=chndx2+len4+1
	      ititl(chndx1:chndx2)=chnum4(1:len4)//')'
	   }
	   else if (flag == 2)
	   {
	      call num2ch (x,len1,chnum1)
	      call num2ch (xdel,len2,chnum2)
	      call num2ch (z,len3,chnum3)
	      call num2ch (zdel,len4,chnum4)
	      chndx1=17
	      chndx2=chndx1+3
	      ititl(chndx1:chndx2)=' px('
	      chndx1=chndx2+1
	      chndx2=chndx2+len1+1
	      ititl(chndx1:chndx2)=chnum1(1:len1)//'+'
	      chndx1=chndx2+1
	      chndx2=chndx2+len2+3
	      ititl(chndx1:chndx2)=chnum2(1:len2)//',*,'
	      chndx1=chndx2+1
	      chndx2=chndx2+len3+1
	      ititl(chndx1:chndx2)=chnum3(1:len3)//'+'
	      chndx1=chndx2+1
	      chndx2=chndx2+len4+1
	      ititl(chndx1:chndx2)=chnum4(1:len4)//')'
	   }
	   else if (flag == 3)
	   {
	      call num2ch (x,len1,chnum1)
	      call num2ch (xdel,len2,chnum2)
	      call num2ch (y,len3,chnum3)
	      call num2ch (ydel,len4,chnum4)
	      chndx1=17
	      chndx2=chndx1+3
	      ititl(chndx1:chndx2)=' px('
	      chndx1=chndx2+1
	      chndx2=chndx2+len1+1
	      ititl(chndx1:chndx2)=chnum1(1:len1)//'+'
	      chndx1=chndx2+1
	      chndx2=chndx2+len2+1
	      ititl(chndx1:chndx2)=chnum2(1:len2)//','
	      chndx1=chndx2+1
	      chndx2=chndx2+len3+1
	      ititl(chndx1:chndx2)=chnum3(1:len3)//'+'
	      chndx1=chndx2+1
	      chndx2=chndx2+len4+3
	      ititl(chndx1:chndx2)=chnum4(1:len4)//',*)'
	   }
	   else
	   {
	      write (6,*)'seth3d: ERROR - var flag set incorrectly'
	      return
	   }
	}
	else
	{
	   if      (flag == 1)
	   {
	      call num2ch (y,len1,chnum1)
	      call num2ch (z,len2,chnum2)
	      chndx1=17
	      chndx2=chndx1+5
	      ititl(chndx1:chndx2)=' px(*,'
	      chndx1=chndx2+1
	      chndx2=chndx2+len1+1
	      ititl(chndx1:chndx2)=chnum1(1:len1)//','
	      chndx1=chndx2+1
	      chndx2=chndx2+len2+1
	      ititl(chndx1:chndx2)=chnum2(1:len2)//')'
	   }
	   else if (flag == 2)
	   {
	      call num2ch (x,len1,chnum1)
	      call num2ch (z,len2,chnum2)
	      chndx1=17
	      chndx2=chndx1+3
	      ititl(chndx1:chndx2)=' px('
	      chndx1=chndx2+1
	      chndx2=chndx2+len1+3
	      ititl(chndx1:chndx2)=chnum1(1:len1)//',*,'
	      chndx1=chndx2+1
	      chndx2=chndx2+len2+1
	      ititl(chndx1:chndx2)=chnum2(1:len2)//')'
	   }
	   else if (flag == 3)
	   {
	      call num2ch (x,len1,chnum1)
	      call num2ch (y,len2,chnum2)
	      chndx1=17
	      chndx2=chndx1+3
	      ititl(chndx1:chndx2)=' px('
	      chndx1=chndx2+1
	      chndx2=chndx2+len1+1
	      ititl(chndx1:chndx2)=chnum1(1:len1)//','
	      chndx1=chndx2+1
	      chndx2=chndx2+len2+3
	      ititl(chndx1:chndx2)=chnum2(1:len2)//',*)'
	   }
	   else
	   {
	      write (ititl(25:40),*)'seth3d: ERROR - var flag set incorrectly'
	      return
	   }
	}

#  Set 60 character history known as ihist in specpr
	ihist='Extraction from 3D file'

#  Set other parameters not important at present
	call fuser (usernm)
	iscta = when(1,ftptr(in))
	isctb = when(1,ftptr(in))
	jdatea = when(2,ftptr(in))
	jdateb = when(2,ftptr(in))
	istb = 0
	isra = 0
	isdec = 0
	irmas = 0
	itchan= n
	revs = 1
	iband(1) = 1
	iband(2) = 1
	irwav = 1
	irespt = 0
	irecno = a
	itpntr =0
	mhist = ' '
	nruns =0
	siangl =0
	seangl =0
	sphase =0
	iwtrns =1
	itimch =1
	xnrm = 1.0
	scatim = .08333333
	timint = scatim
	tempd = 273.

#  End and return to calling program
	return
	end
