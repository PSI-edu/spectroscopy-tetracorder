	
	SUBROUTINE seth3d (in,flag,n,a)

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
#             titl3d - title input by user for 3d file extraction  *
#                                                                  *
#                                                                  *
#                                                                  *
#                                                                  *
#                                                                  *
#                                                                  *
#                                                                  *
#*******************************************************************

#  Set variable type
	IMPLICIT INTEGER*4 (i-n)
	INTEGER*4      n,a,x,y,z,flag,in,test
	INTEGER*4      len1,len2,len3,len4
	CHARACTER*10   chnum1,chnum2,chnum3,chnum4
	INCLUDE        ../common/label1
	INCLUDE        ../common/ioftyp
	INTEGER*2      ibit

#  Set bit flags
	ibit = 0
	CALL clrbit(icflag,ibit)
	ibit = 1
	CALL clrbit(icflag,ibit)
	ibit = 3
	CALL setbit(icflag,ibit)

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

	IF (test != -1)  {
	   CALL num2ch (x,
	   IF      (flag == 1) WRITE (ititl(17:40),103) y,ydel,z,zdel
	   ELSE IF (flag == 2) WRITE (ititl(17:40),104) x,xdel,z,zdel
	   ELSE IF (flag == 3) WRITE (ititl(17:40),105) x,xdel,y,ydel
	   ELSE {
	      WRITE (6,*)'seth3d: ERROR - var flag set incorrectly'
	      RETURN
	   }
	}
	ELSE  {
	   IF      (flag == 1) WRITE (ititl(25:40),100) y,z
	   ELSE IF (flag == 2) WRITE (ititl(25:40),101) x,z
	   ELSE IF (flag == 3) WRITE (ititl(25:40),102) x,y
	   ELSE {
	      WRITE (ititl(25:40),*)'seth3d: ERROR - var flag set incorrectly'
	      RETURN
	   }
	}

100	FORMAT (' px(*,',i4,',',i4,')')
101	FORMAT (' px(',i4,',*,',i4,')')
102	FORMAT (' px(',i4,',',i4,',*)')
103	FORMAT (' px(*,',i4,'+',i3,',',i4,'+',i3,')')
104	FORMAT (' px(',i4,'+',i3,',*,',i4,'+',i3,')')
105	FORMAT (' px(',i4,'+',i3,',',i4,'+',i3,',*)')
	
#  Set 60 character history known as ihist in specpr
	ihist='Extraction from 3D file'

#  Set other parameters not important at present
	CALL fuser (usernm)
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
	RETURN
	END
