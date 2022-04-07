	subroutine redbil (chbuff,i2buff,i4buff,r4buff,ioerr,
                        xel,yel,zel,flag,in,n,a)
	# SCCS ID: %Z% %W% %G%

#...................................................................
#   TITLE:           3D READ ROUTINE FOR BIL FORMAT DATA           .
#...................................................................
#   DESCRIPTION:                                                   .
#               This routine is designed to read data files which  .
#               are in band interleaved by line (BIL) format.      .
#               In this format the spectral axis is the z axis.    .
#               The records to be read are chosen by equations     .
#               which depend on the extraction direction, coor-    .
#               dinates of the line array containing the spectra   .
#               and the parameters of the input file like dimen-   .
#               sions of the array, records per line, pixels per   .
#               record, etc.  Spectra ends up in <data> array.     .
#...................................................................
#   VARIABLES:                                                     .
#             a,b,c    - do loop parameters for record reading     .
#             x,y,zel  - coordinates of the line array to be con-  .
#                      sidered as the spectra                      .
#             i*,r4buff- buffer arrays for holding records         .
#             NOTE: Other variables are described in ioftyp.h      .
#                                                                  .
#...................................................................

#  Set variable type
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4      ioerr,xel,yel,zel,n,a,b,c,filhdr,reclen
	integer*4      dx,dy,dz,hdrlen,flag,in,type,rechdr,pixrec
	integer*4      orgniz,reclin,dnoff,i4buff(384)
	integer*2      i2buff(768)
	real*4         r4buff(384),scale,ptdrop
	character      chbuff*1536
	include        "../common/label1"
	include        "../common/ioftyp"
	include        "../common/lundefs"

#  Initialize file parameter variables
	orgniz = filtyp(10,ftptr(in))
	filhdr = filtyp(2,ftptr(in))
	reclen = filtyp(3,ftptr(in))
	rechdr = filtyp(4,ftptr(in)) + 1
	dnoff = filtyp(5,ftptr(in))
	scale = dnscal(ftptr(in))
	dx = filtyp(7,ftptr(in))
	dy = filtyp(6,ftptr(in))
	dz = filtyp(8,ftptr(in))
	type = filtyp(9,ftptr(in))
	ptdrop = float(filtyp(11,ftptr(in)))
	n = 1
	
#  Initialize pixels per record and records per line variables
	if (type == 1)  {
	   pixrec = reclen / 2
	}
	else if (type == 2 | type == 3)  {
	   pixrec = reclen / 4
	}
	reclin = dx / pixrec
	k = mod(dx, pixrec)

	if (reclen == 0 | k != 0) {
	   write (ttyout,101) pixrec
101        format ('Sample dimension must be a multiple of ',i6)
	   return
	}

#  Prompt for the coordinates of the line array and read array
	if (flag == 1)  { # Read all x values
	#  Equations to control which records are read 
    	   a = (yel - 1) *  dz * reclin + (zel - 1) * reclin + 1 + filhdr
	   b = a + reclin - 1
	   c = 1
	
	#  Read and extract spectra as designated
	   do i = a, b, c  {
	      read (in, REC = i,IOSTAT = ioerr) chbuff(rechdr:reclen)

	      if (type == 1)  {
	         do j = 1, pixrec  {
	            if (i2buff(j) == ptdrop) { data(n) = -1.23e34 }
		    else { data(n) = scale * float(i2buff(j) + dnoff) }
		    n = n + 1
	         }
	      }
	      else if (type == 2)  {
	         do j = 1, pixrec  {
	            if (i4buff(j) == ptdrop) { data(n) = -1.23e34 }
		    else { data(n) = scale * float(i4buff(j) + dnoff) }
		    n = n + 1
	         }
	      }
	      else  {
	         do j = 1, pixrec  {
	            if (r4buff(j) == ptdrop) { data(n) = -1.23e34 }
		    else { data(n) = scale * (r4buff(j) + dnoff) }
		    n = n + 1
	         }
	      }
	   }
	}

	else if (flag == 2)  { # Read all y

	#  Equations to control which records are read
	   k = mod(xel, pixrec)
	   a = (zel - 1) * reclin + xel / pixrec + filhdr
	   if (k != 0) { a = a + 1 }
	   else { k = pixrec }
	   b = dy * dz * reclin
	   c = reclin * dz

	#  Read and extract spectra as designated
	   do i = a, b, c {
	      read (in, REC = i, IOSTAT = ioerr) chbuff(rechdr:reclen)

	      if (type == 1)  {
	         if (i2buff(xel) == ptdrop) { data(n) = -1.23e34 }
		 else { data(n) = scale * float(i2buff(xel) + dnoff) }
	      }
	      else if (type == 2)  {
	         if (i4buff(xel) == ptdrop) { data(n) = -1.23e34 }
		 else { data(n) = scale * float(i4buff(xel) + dnoff) }
	      }
	      else  {
	         if (r4buff(xel) == ptdrop) { data(n) = -1.23e34 }
	    	 else { data(n) = scale * (r4buff(xel) + dnoff) }
	      } 
	      n = n + 1
	   }
	}         

	else if (flag == 3)  { # Read all z
	#  Equations to read correct records
	   k = mod(xel, pixrec)
	   a = xel / pixrec + (yel - 1) * dz * reclin + filhdr
	   if (k != 0) { a = a + 1 }
	   else { k = pixrec }
	   b = a + (dz - 1) * reclin
	   c = reclin
	
	#  Read records and extract spectra as designated
	   do i = a, b, c  {
	      read (in, REC = i,IOSTAT = ioerr) chbuff(rechdr:reclen)

	      if (type == 1)  {
	         if (i2buff(k) == ptdrop) { data(n) = -1.23e34 }
		 else { data(n) = scale * float(i2buff(k) + dnoff) }
	      }
	      else if (type == 2)  {
	         if (i4buff(k) == ptdrop) { data(n) = -1.23e34 }
	  	 else { data(n) = scale * float(i4buff(k) + dnoff) }
	      }
	      else  {
	         if (r4buff(k) == ptdrop) { data(n) = -1.23e34 }
		 else { data(n) = scale * (r4buff(k) + dnoff) }
	      }
	      n = n + 1
	   }
	}
	else  {
	   return
	}

#  Set variable for number of channels read and first record read
	itchan = n - 1
	n = n - 1
	irecno = a
	
#  Return to main
	return
	end
