	subroutine read3d (in,ioerr)   # SCCS ID: %Z% %W% %G%

#*******************************************************************
#   TITLE:                3D FILE READ DRIVER                      *
#   PROGRAMMER:           Barry J. Middlebrook                     *
#------------------------------------------------------------------*
#  DESCRIPTION:                                                    *
#              This routine drives the subroutines for reading a   *
#              3d data file.  It assumes that the file parameters  *
#              are preset and that the user has indicated the      *
#              pixel coordinates and extraction direction previous *
#              to entering this routine.  The subroutine that the  *
#              data cube read program flow is directed to is keyed *
#              on the file organization.  The allowable type of    *
#              file organizations are band interleaved by line     *
#              (BIL), band interleaved by pixel (BIP) and band     *
#              sequential (BSQ).                                   *
#                                                                  *
#------------------------------------------------------------------*
#  VARIABLES:                                                      *
#            filreq      - file required, an array which indicates *
#                        the pixel coordinates and extraction dir- *
#                        ection and helps determine which record   *
#                        to read (many people think of records as  *
#                        files)                                    *
#            ftype       - 12x5 array which contains file para-    *
#                        meters for 5 files                        *
#            orgniz      - organization of file (BIL,BSQ,BIP)      *
#            BIL         - subr to read BIL format                 *
#            BIP         - subr to read BIP format                 *
#            BSQ         - subr to read BSQ format                 *
#            chbuff      - character buffer to enable reading      *
#                        specific bytes of data                    *
#            i2buff      - integer half word array (2 byte)        *
#            i4buff      - integer full word array (4 byte)        *
#            r4buff      - real full word array                    *
#            iobuff      - integer full word array                 *
#                                                                  *
#*******************************************************************

#  Set variable type and include common data blocks
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4      ioerr,flag,x,y,z,orgniz,n,a,in,test
	integer*4      i4buff(384)
	integer*2      i2buff(768)
	real*4         r4buff(384)
	character      chbuff*1536,name*8
# ***TESTING***
	include		"../common/dscrch"
	include		"../common/lbl3"
# ***END TEST***

	include        "../common/ioftyp"
	include        "../common/label1"
	include        "../common/label3"
	include        "../common/lundefs"
	equivalence    (chbuff,i2buff,i4buff,r4buff,iobuff)

#  Get the extraction direction and set variable flag
	x=filreq(2)
	y=filreq(1)
	z=filreq(3)

	if (x==-1) flag=1
	if (y==-1) flag=2
	if (z==-1) flag=3

#  Direct program flow to correct read routine
	orgniz=filtyp(10,ftptr(in))
	test=fiobox(1)+fiobox(2)+fiobox(3)

	if (test == -1)  {
	   idad=0

	   if (orgniz == 1)  {
	      call redbil (chbuff,i2buff,i4buff,r4buff,ioerr,x,y,z,flag,in,n,a)
	   }
	   else if (orgniz == 2)  {
	      call redbip (chbuff,i2buff,i4buff,r4buff,ioerr,x,y,z,flag,in,n,a)
	   }
	   else  {
	      call redbsq (chbuff,i2buff,i4buff,r4buff,ioerr,x,y,z,flag,in,n,a)
   	   }
	}

	else  {
	   idad=2
	   call blkext (chbuff,i2buff,i4buff,r4buff,ioerr,x,y,z,flag,in,n,a)
	}

#  Call header information routine
	call seth3d (in,flag,n,a)
	
#  Return to specpr redfil
	return
	end
