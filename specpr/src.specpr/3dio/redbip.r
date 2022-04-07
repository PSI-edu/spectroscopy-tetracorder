	subroutine redbip (chbuff,i2buff,i4buff,r4buff,ioerr,
                        xel,yel,zel,flag,in,n,a)
	# SCCS ID: %Z% %W% %G%

#...................................................................
#   TITLE:             3D FILE READ FOR BIP FORMAT DATA FILE       .
#...................................................................
#   DESCRIPTION:                                                   .
#               This part reads data files which are in band inter-.
#    	        leaved by pixel (BIP) format.  The records read    .
#               are chosen by an equation which depends on the ex- .
#               traction direction, the coordinates of the desig-  .
#               nated line array and the parameters of the data    .
#               file read (dimensions of the 3-d array, records    .
#               per line, pixels per record,...,etc.).  This for-  .
#               mat (BIP) has the z axis as the spectral axis.     .
#               Spectrum will end up in <data> array.              .
#...................................................................
#   VARIABLES:                                                     .
#             a,b,c    - do loop parameters                        .
#             xel      - x element position or coordinate for the  .
#                      purpose of locating the spectra to be ex-   .
#                      tracted                                     .
#             yel      - y coordinate                              .
#             zel      - z coordinate                              .
#             n        - output array subscript                    .
#...................................................................


#  Set variable type
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	integer*4      xel,yel,zel,n,a,b,c,k,m,filhdr,reclen
	integer*4      dx,dy,dz,hdrlen,flag,in,type,rechdr,pixrec
	integer*4      orgniz,dnoff,i4buff(384)
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
	reclin = dz / pixrec
	k = mod(dz, pixrec)

	if (reclin == 0 | k != 0)  {
	   write (ttyout,*)'z dimension should be a multiple of ',pixrec
	   return
	}

#  Prompt for the coordinates of the line array and read array
	if (flag == 1)  { # Read all x
	
	#  Equations to control which records are read
	   k = mod(zel, pixrec)
    	   a = (yel - 1) * dx * reclin + zel / pixrec + filhdr
	   if (k != 0) { a = a + 1 }
	   else { k = pixrec }
	   b = a + dx * reclin - 1
	   c = reclin
	
	#  Loop to read records and extract spectrum
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
	         else {data(n) = scale *(r4buff(k) + dnoff) }
	      }
	      n = n + 1
	   }
	}

	else if (flag == 2)  { # Read all y

	#  Equations to control which records are read
	   k = mod(zel, pixrec)
	   a = (xel - 1) * reclin + zel / pixrec + filhdr
	   if (k != 0) { a = a + 1 }
	   else { k = pixrec }
	   b = a + (dy - 1) * reclin * dx
	   c = reclin * dx

	#  Read and extract spectrum as designated
	   do i = a, b, c {
	      read (in, REC = i, IOSTAT = ioerr) chbuff(rechdr:reclen)

	      if (type == 1 )  {
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
	          
	else if (flag == 3)  { # Read all z

	#  These equations to control which records are read
	   a = (yel - 1) * reclin * dx + (xel - 1) * reclin + 1 + filhdr
	   b = a + reclin - 1
	   c = 1
	
	#  Read the records and extract spectra
	   do i = a, b, c  {
	      read (in, REC = i, IOSTAT = ioerr) chbuff(rechdr:reclen)

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

	else  {
	   return
	}

#  Set variables for number of channels read and first record read
	itchan = n - 1
	n = n - 1
	irecno = a

#  Return to main program
	return
	end
