	subroutine redbsq (chbuff,i2buff,i4buff,r4buff,ioerr,
                        xel,yel,zel,flag,in,n,a)
	# SCCS ID: %Z% %W% %G%

#...................................................................
#   TITLE:           3-DIMENSIONAL DATA READ ROUTINE FOR BSQ       .
#...................................................................
#   DESCRIPTION:                                                   .
#               This routine reads a 3-d data file which is or-    .
#               ganized in band sequential format.  This format    .
#               can be visualized as having the records lined up   .
#               parallel to the x-axis.  The records read are de-  .
#               termined by an equation which depends on the ex-   .
#               traction direction, the coordinates of the line    .
#               array to be considered as the spectra, and the     .
#               parameters of the files like pixels per record,    .
#               records per line and dimensions of the input       .
#               array.  Output array is data.                      .
#...................................................................
#   VARIABLES:                                                     .
#             flag      - indicates what type of numbers are read  .
#             a,b,c     - do loop parameters for reading records   .
#             n         - output array subscript                   .
#             x,y,zel   - coordinates of line array to be con-     .
#                       sidered as the spectra                     .
#             i*,r4buff - buffer array to hold records read        .
#                                                                  .
#...................................................................

#  Set variable type
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4      xel,yel,zel,n,a,b,c,filhdr,reclen,dx,dy,dz
	integer*4      hdrlen,flag,in,type,rechdr,pixrec,orgniz,reclin
	integer*4      dnoff,i4buff(384)
	integer*2      i2buff(768) 
	real*4         r4buff(384),ptdrop,scale
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
	ptdrop = FLOAT(filtyp(11,ftptr(in)))
	n = 1

#  Initialize pixels per record and records per line variables
	if (type == 1)  {
	   pixrec = reclen / 2
	}
	else if (type == 2 | type == 3)  {
	   pixrec = reclen / 4
	}
	reclin = dx / pixrec
	k = MOD(dx, pixrec)

	if (reclin == 0 | k != 0)  {
	   write (ttyout,*)'x dimension must be a multiple of ',pixrec
	   return
	}

#  Prompt user for location of array to be read and read line array
	if (flag == 1)  { # Read all x

	#  Equations to control record read loop
	   a = (zel - 1) * reclin * dy + (yel - 1) * reclin + 1 + filhdr
	   b = a + reclin - 1
	   c = 1
	   
	#  Read records and extract spectra
	   do i = a, b, c {
	      read (in, REC = i, IOSTAT = ioerr) chbuff(rechdr:reclen)

	      if (type == 1 )  {
	         do j = 1, pixrec  {
	            if (i2buff(j) == ptdrop) { data(n) = -1.23e34 }
	            else { data(n) = scale * FLOAT(i2buff(j) + dnoff) }
		    n = n + 1
	         }
	      }
	      else if (type == 2)  {
	         do j = 1, pixrec  {
	            if (i4buff(j) == ptdrop) { data(n) = -1.23e34 }
	            else { data(n) = scale * FLOAT(i4buff(j) + dnoff) }
		    n = n + 1
	         }
	      }
	      else  {
	         do j = 1, pixrec  {
	            if (r4buff(j) == ptdrop) { data(n) = -1.23e34 }
	            else { data(n) = scale * r4buff(j) + dnoff }
		    n = n + 1
	         }
	      } 
	   }
	}
         
	else if (flag == 2)  { # Read all y

	#  Equations to control which records are read
	   k = mod(xel, pixrec)
    	   a = (zel - 1) * dy * reclin + xel / pixrec + filhdr
	   if (k != 0) { a = a + 1 }
	   else { k = pixrec }
	   b = reclin * dy
	   c = reclin
	
	#  Read file records and extract spectra as designated
	   do i = a, b, c  {
	      read (in, REC = i, IOSTAT = ioerr) chbuff(rechdr:reclen)

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
	         else { data(n) = scale * r4buff(k) + dnoff }
	      }
	         n = n + 1
	   }
	}

	else if (flag == 3)  { # Read all z

	#  Equations to control which records are read
	   k = mod(xel, pixrec)
	   a = (yel - 1) * reclin + xel / pixrec + filhdr
	   if (k != 0) { a = a + 1 }
	   else { k = pixrec }
	   b = dy * dz * reclin
	   c = reclin * dy
	
	#  Read records and extract spectra
	   do i = a, b, c  {
	      read (in, REC = i, IOSTAT = ioerr) chbuff(rechdr:reclen)

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

#  Initialize variables for number of channels read and first record read
	itchan = n - 1
	irecno = a
	n = n - 1

#  Now return to main program
	return
	end
