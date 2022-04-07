	subroutine file3d (in,infile,ioerr,il)
	# SCCS ID: %Z% %W% %G%

#*******************************************************************
#   TITLE:        3-D FILE PARAMETER SETUP ROUTINE                 *
#                                                                  *
#   PROGRAMMER:   Barry J. Middlebrook                             *
#                 USGS Branch of Geophysics                        *
#                 Denver West Offices                              *
#                 (303) 236-1411                                   *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               This program is designed for setting up i/o of 3-  *
#               dimensional data.  The goal is to allow the user   *
#               to designate the direction of extraction from the  *
#               cube.  The user needs to have knowledge of the     *
#               file parameters such as header length, record      *
#               length and how the data is organized.  Included    *
#               in the program are simple graphics to help the     *
#               user visualize exactly which dimensions are being  *
#               used for the spectrum and wavelength.              *
#                                                                  *
#------------------------------------------------------------------*
#   OPERATION:                                                     *
#             1)  User sets up all file parameters which control   *
#             how the data file is read.                           *
#             2)  User verifies that the file parameters are cor-  *
#             rect.                                                *
#             3)  Program opens the data file then displays the    *
#             graphics to help the user visualize the extraction   *
#             direction.                                           *
#             4)  Once the user designates the extraction direc-   *
#             tion a cube is displayed to indicate how the pro-    *
#             gram will read the data.                             *
#             5)  The records to be used are selected by an eq-    *
#             uation which depends on the file parameters and the  *
#             coordinates of the spectra to be extracted.          *
#             6)  Subroutines are categorized under the BIL, BSQ   *
#             or BIP file organizations.                           *
#             7)  Records are read into a character buffer array   *
#             which is equivalenced to a halfword integer, full-   *
#             word integer and fullword real array.                *
#             8)  Data is converted to fullword real and loaded    *
#             into a specpr working array.                         *
#                                                                  *
#------------------------------------------------------------------*
#         NOTES:                                                   *
#               2/3/88    -Barry-                                  *
#               Currently, the record header length is assumed to  *
#               be zero.                                           *
#                                                                  *
#               Program will allow for more than one record/line.  *
#                                                                  *
#               Intrinsic function which converts i4 to r4 also    *
#               converts i2 to r4 (don't ask me how this happens). *
#                                                                  *
#               3/16/88     -Barry-                                *
#               Previously this program was part of Cuberead but   *
#               is now modified for inclusion into specpr.         *
#                                                                  *
#               11/27/89   -Barry-                                 *
#               Now modified to perform block extractions.         *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     *
#             in       - input file unit number                    *
#             filhdr   - file header length in records             *
#             reclen   - record length in bytes                    *
#             hdrlen   - file header length in bytes               *
#             rechdr   - record header length in bytes             *
#             dx       - element dimension of x axis               *
#             dy       - element dimension of y axis               *
#             dz       - element dimension of z axis               *
#             type     - type of values in data file               *
#             flag     - indicates the extraction direction for    *
#                      the spectra                                 *
#             orgniz   - organization of data file (BIL,BSQ or     *
#                      BIP)                                        *
#             dnoff    - data number offset                        *
#             scale    - data number scale                         *
#             pixrec   - pixels per record                         *
#             reclin   - records per line (careful!  This is close *
#                      to the variable reclen)                     *
#             dattyp   - data type character variable              *
#             chorg    - organization character variable           *
#             infile   - input file name                           *
#             label1   - common block name from specpr main pro-   *
#                      gram which provides the link between this   *
#                      program and specpr                          *
#             chbuff   - character buffer array to read data file  *
#             i2buff   - two byte-integer array which serves as    *
#                      temporary storage for reading the input     *
#                      data records                                *
#             i4buff   - four byte-integer number array            *
#             r4buff   - four byte-real number array               *
#             ptdrop   - point drop flag (currently, for AVIRIS    *
#                        flag value is 4096)                       *
#                                                                  *
#*******************************************************************

#  Set variable type
	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	real*4 x
	integer*4        reclen,tmpnum
	character      chorg*3,dattyp*9,infile*80,cmt*13
	include        "../common/label1"
	include        "../common/ioftyp"
	include        "../common/lundefs"
	include        "../common/alphabet"

	cmt='File not open'
	chorg='***'
	dattyp='strnotset'

# Check that input logical unit is valid
	if (ftptr(in) < 1 | ftptr(in) > 5) {
		write (ttyout,*)'INVALID LOGICAL UNIT NUMBER'
                write (ttyout,*)'- exiting 3D setup routines...'
		ioerr = 1
		return
	}

#  Give user information on routine
	write (ttyout,*)'---------------------------------------------------'
	write (ttyout,*)'This program is designed to read multidimensional'
	write (ttyout,*)'data files although at present it is limited to 3-'
	write (ttyout,*)'dimensional data sets.  It is recommended that you'
	write (ttyout,*)'put the input file parameters in a command file to'
	write (ttyout,*)'save time if you find it necessary to change the'
	write (ttyout,*)'specpr u,v,w,y or d setups often.  Currently,'
	write (ttyout,*)'it takes 12 parameters to set up a file.'
	write (ttyout,*)'                 *************'
	write (ttyout,*)'NOTE:  AT ANY TIME entering an e or x terminates'
	write (ttyout,*)'the setup routines without opening the 3d file.'
	write (ttyout,*)'---------------------------------------------------'
	write (ttyout,*)

#                      ---------------------------------
#                     | PROMPT USER FOR FILE PARAMETERS |
#                      ---------------------------------

#  Prompt for record length
1	write (ttyout,*)'What is the length of each record (in bytes)?'
	write (ttyout,*)'(e or x to exit)'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il != 0) {
 	   write (ttyout,*)'Entry is invalid.'		
	   go to 1	
	}
	if (x < 1 | x > 1536) {
	   write (ttyout,*)'ERROR: record length must range from 1-1536 bytes'
	   go to 1	
	}

#  Set file parameter
	filtyp(3,ftptr(in))=ifix(x)
	reclen=filtyp(3,ftptr(in))

#  Open 3d input file
	write (ttyout,*)
	write (ttyout,*)'Opening file...'
	write (ttyout,*)
	open (UNIT=in,
              FILE=infile,
              RECL=reclen,
	      ACCESS='DIRECT',
	      STATUS='OLD',
	      IOSTAT=ioerr,
	      ERR=15)

15	if (ioerr == 918) { cmt='File open' ; go to 11 }
	else if (ioerr != 0)  { return }
	else  {
	   write (ttyout,*)'File opened successfully...'
	   cmt='File opened'
	}

	write (ttyout,*)
	write (ttyout,*)
	   
#  Remove path from name and put into history
	call rmpath(infile,name)
	call namdev(in,name)

#  Prompt for organization
2	write (ttyout,*)'Select file organization:'
	write (ttyout,*)'                   1   BIL (band interleaved by line)'
        write (ttyout,*)'                   2   BIP (band interleaved by pixel)'
 	write (ttyout,*)'                   3   BSQ (band sequential)'
	write (ttyout,*)'                   s   skip to parameter check list'
	write (ttyout,*)'                   e,x exit program'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il == ihb) go to 1
	if (il == ihs) go to 11
	if (il != 0) {
	   write (ttyout,*)'Input value not allowed, try again.'	
           go to 2	
	}
	if (x < 1 | x > 3) {
		write (ttyout,*)'Value out of range, try again.'
		go to 2
	}

#  Set file parameter
	filtyp(10,ftptr(in))=ifix(x)

#  Set character string to echo user input in parameter list
        if (filtyp(10,ftptr(in)) == 1) {
          chorg ='BIL'
	}
	else if (filtyp(10,ftptr(in)) == 2) {
          chorg ='BIP'
	}
	else if (filtyp(10,ftptr(in)) == 3) {
          chorg ='BSQ'
	}
	else {
	   go to 2
	}

#  Prompt for file header length
3	write (ttyout,*)'How many records long is the file header?'
	write (ttyout,*)'(e or x to exit, b to back up, s to skip to list)'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il == ihb) go to 2
	if (il == ihs) go to 11
	if (il != 0) {
	   write (ttyout,*)'Improper decode, try again.'	
	   go to 3	
	}

#  Set file parameter
	filtyp(2,ftptr(in))=ifix(x)

	if (x < 0) { write (ttyout,*)'Invalid input'; go to 3 }

#  Prompt for record header length
4	write (ttyout,*)'What is the length of the record header in bytes?'
	write (ttyout,*)'(e or x to exit, b to back up, s to skip to list)'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il == ihb) go to 3
	if (il == ihs) go to 11
	if (il != 0) {
 	   write (ttyout,*)'Input value misunderstood, try again.'		
	   go to 4
	}
	if (x < 0 | x > reclen) { write (ttyout,*)'Invalid input'; go to 4 }
	if (x >= reclen)  {
	   write (ttyout,*)'Record header length is >= record length.'
	   go to 4
	}
#  Set file parameter
 	filtyp(4,ftptr(in))=ifix(x)

#  Prompt for Data Number offset
5	write (ttyout,*)'What is the DN offset?'
	write (ttyout,*)'(e or x to exit, b to back up, s to skip to list)'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il == ihb) go to 4
	if (il == ihc) go to 11
	if (il != 0) {
 	   write (ttyout,*)'Improper decode, try again.'		
	   go to 5
	}
	if (x < 0) { write (ttyout,*)'Invalid input'; go to 5 }

#  Set file parameter
	filtyp(5,ftptr(in))=ifix(x)

#  Prompt for Data Number scale
6	write (ttyout,*)'What is the DN scale?'
	write (ttyout,*)'(e or x to exit, b to back up, s to skip to list)'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il == ihb) go to 5
	if (il == ihc) go to 11
	if (il != 0) {
 	   write (ttyout,*)'Improper decode, try again.'		
	   go to 6
	}
	if (x <= 0) { write (ttyout,*)'Invalid input'; go to 6 }
	dnscal(ftptr(in))=x

#  Print visual aids for user
7	write (ttyout,*)'                     ......................'
	write (ttyout,*)'                   .                    . .'
	write (ttyout,*)'  ................                    .   .'
	write (ttyout,*)'.....................................     .'
	write (ttyout,*)'. File header  .                    .     .'
	write (ttyout,*)'................                    .     .'
	write (ttyout,*)'               .                    .     .'
	write (ttyout,*)'        Lines  .       data         .     .'
	write (ttyout,*)'               .                    .     . s'
	write (ttyout,*)'               .                    .   . d'
	write (ttyout,*)'               .                    . . n'
	write (ttyout,*)'               ...................... a'
	write (ttyout,*)'                       Samples      B'
	write (ttyout,*)


#  Prompt for dimensions of the data cube
	write (ttyout,*)'Please enter the dimensions of the input array:'
	write (ttyout,*)'(e or x to exit, b to back up, s to skip to list)'
	write (ttyout,*)'nL nS nB'

#  Get and decode user input
	call crtin
	i=1
	n=6

#  Loop to get 3 dimensions for array
	do j=1,3  {
	   call wjfren (i,x,il)

	#  Check for erroneous input and either return to specpr, back up to
	#  correct or skip to paramater list
	   if (il == ihe | il == ihx & j == 1) return
	   if (il == ihb) go to 6
	   if (il == ihs) go to 11
	   if (il != 0) {
	     write (ttyout,*)'Invalid input, try again.'
  	     go to 7
	   }
	   if (x < 1 | x > 9999) {
              call what(i)
              write (ttyout,*)'Value out of range, reenter.'
              go to 7
           }
	   #  Set file parameter
	   filtyp(n,ftptr(in))=ifix(x)
	   n = n + 1
	}

#  Prompt for data value type
8	write (ttyout,*)'Choose the file data type:'
	write (ttyout,*)'--------------------------'
	write (ttyout,*)
	write (ttyout,*)'1   Integer*2 (half-word) '
	write (ttyout,*)'2   Integer*4 (full-word) - specpr normal'
	write (ttyout,*)'3   Real*4 (full-word)'
	write (ttyout,*)
	write (ttyout,*)'b   backup'
	write (ttyout,*)'s   skip to parameter check list'
	write (ttyout,*)'e,x to exit'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il == ihb) go to 7
	if (il == ihs) go to 11
	if (il != 0) {
 	   write (ttyout,*)'Improper decode, try again.'		
	   go to 8
	}
	if (x < 0 | x > 3) { write (ttyout,*)'Invalid input'; go to 8 }

#  Set file parameter
	filtyp(9,ftptr(in))=ifix(x)

	if (filtyp(9,ftptr(in)) == 1)  {
	  dattyp='Integer*2'
	}
	else if (filtyp(9,ftptr(in)) == 2)  {
	  dattyp='Integer*4'
	}
	else if (filtyp(9,ftptr(in)) == 3)  {
	  dattyp='Real*4'
	}
	else  {
	   go to 8
	}

#  Prompt for point dropout value to flag dropouts 
9 	write (ttyout,*)'What is the deleted point value?'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  correct or skip to paramater list
	if (il == ihe | il == ihx) return
	if (il == ihb) go to 8
	if (il != 0) {
           call what(i)
 	   write (ttyout,*)'Input is invalid, try again.'		
	   go to 9
	}
#  Set file parameter
	filtyp(11,ftptr(in))=ifix(x)
	
#  Test to see if the user input the dimensions correctly
11	write (ttyout,*)'Please carefully check your input:'
	write (ttyout,*)'---------------------------------------'
	write (ttyout,*)'Data file: ',infile
	write (ttyout,*)'Parm.#'
	write (ttyout,*)'                  Status =  ',cmt
	write (ttyout,*)' 1  Record Header length = ',filtyp(4,ftptr(in)) 
	write (ttyout,*)' 2                Format =  ',chorg
	write (ttyout,*)' 3    File Header length = ',filtyp(2,ftptr(in))
	write (ttyout,*)' 4         Record length = ',filtyp(3,ftptr(in))
	write (ttyout,*)' 5             DN offset = ',filtyp(5,ftptr(in))
	write (ttyout,*)' 6              DN scale = ',dnscal(ftptr(in))
	write (ttyout,*)' 7                 Lines = ',filtyp(6,ftptr(in))
	write (ttyout,*)'                 Samples = ',filtyp(7,ftptr(in))
	write (ttyout,*)'                Channels = ',filtyp(8,ftptr(in))
	write (ttyout,*)' 8             Data type =  ',dattyp
	write (ttyout,*)' 9       Point drop flag = ',filtyp(11,ftptr(in))
	write (ttyout,*)'---------------------------------------'
	write (ttyout,*)
	write (ttyout,*)

	write (ttyout,*)'Enter the parameter # to be changed'
        write (ttyout,*)'c     to CONTINUE'
	write (ttyout,*)'e,x   to EXIT file setup'

#  Get and decode user input
	call crtin
	i=1
	call wjfren (i,x,il)

#  Check for erroneous input and either return to specpr, back up to
#  incorrect parameter, or continue and open file 
	if (il == ihe | il == ihx) return
	if (il == 0 & (x < 2 | x > 10)) {
           write (ttyout,*)'Value out of range'
	   go to 11
	}
        else if (il == 0 & x >= 1. & x <= 9.) {
           i=ifix(x)
	   go to (1,2,3,4,5,6,7,8,9) i
	}
	else if (il == ihc)  {
           write (ttyout,*)'Continuing...'
	}
	else {
	   go to 11
	}

#  Set protection to read only
# *** THIS NEEDS TO BE DONE ***	
	
#  Setup rest of header 
	call hedr3d (in)

#  End file parameter setup routine
	return
	end
