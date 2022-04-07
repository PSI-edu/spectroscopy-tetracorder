#*******************************************************************
#   TITLE:                      CUBE READ                          *
#                                                                  *
#   PROGRAMMER:   Barry J. Middlebrook                             *
#                 USGS Branch of Geophysics                        *
#                 Denver West Offices                              *
#                 (303) 236-1411                                   *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               This program is designed to allow i/o of three     *
#               dimensional arrays.  The goal is to allow the user *
#               to designate the array dimension to be used as the *
#               spectrum.  The user needs to have knowledge of the *
#               file parameters such as header length, record      *
#               length and how the array is organized.  Included   *
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
#               2/3/88                                             *
#               Currently, the record header length is assumed to  *
#               be zero.                                           *
#                                                                  *
#               Program will allow for more than one record/line.  *
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
#             io3d     - common block carrying most program values *
#                                                                  *
#*******************************************************************

#  Set variable type
INCLUDE        io3d.h
INCLUDE        label1
EQUIVALENCE    (chbuff,i2buff,i4buff,r4buff)

#  Initialize unit numbers for input file and terminal
ttyin=5
ttyout=6
in=11

#  Give user information on routine
	WRITE (ttyout,*)'---------------------------------------------------'
	WRITE (ttyout,*)'This program is designed to read multidimensional'
	WRITE (ttyout,*)'arrays although at present it is limited to 3 and 2'
	WRITE (ttyout,*)'dimensional data sets.  It is non-destructive to'
	WRITE (ttyout,*)'the input file and allows for extraction directions'  
	WRITE (ttyout,*)'along any axis.  Put the file parameters in a '
	WRITE (ttyout,*)'command file if you need to change files often.'
	WRITE (ttyout,*)'Currently it takes 12 parameters to set up a file.'
	WRITE (ttyout,*)'---------------------------------------------------'
	WRITE (ttyout,*)

#  Prompt user for file parameters

2	WRITE (ttyout,*)'Select file organization:'
	WRITE (ttyout,*)'                    1  BIL (band interleaved by line)'
        WRITE (ttyout,*)'                    2  BIP (band interleaved by pixel)'
	WRITE (ttyout,*)'                    3  BSQ (band sequential)'

	CALL crtin
	i=1
	CALL wjfren (i,x,il)
	IF (il == ihe | il == ihx) {  # exit if an e or x encountered
	   RETURN
	   STOP 	
	}
	IF (il == 'r') GO TO 2
	IF (il != 0) {
	   WRITE (ttyout,*)'No input, try again.'	
           GO TO 2	
	}
	orgniz=x
#	filtyp(k,1)=orgniz

        IF (orgniz == 1) {
          chorg ='BIL'
	}
	ELSE IF (orgniz == 2) {
          chorg ='BIP'
	}
	ELSE IF (orgniz == 3) {
          chorg ='BSQ'
	}
	ELSE {
	   GO TO 1
	}

3	WRITE (ttyout,*)'How many records long is the file header?'
#	READ (ttyin,*)filhdr
	CALL crtin
	i=1
	CALL wjfren (i,x,il)
	IF (il == ihe | il == ihx) {  # exit if an e or x encountered
	   RETURN
	   STOP
	}
	IF (il == 'r') GO TO 3
	IF (il != 0) {
	   WRITE (ttyout,*)'No input, try again.'	
	   GO TO 3	
	}
	filhdr=x
#	filtyp(k,2)=filhdr

	IF (filhdr < 0) { WRITE (ttyout,*)'Invalid input';GO TO 3}

4	WRITE (ttyout,*)'What is the length of each record (in bytes)?'
	CALL crtin
	i=1
	CALL wjfren (i,x,il)
	IF (il == ihe | il == ihx) {  # exit if an e or x encountered
	   RETURN
	   STOP
	}
	IF (il == 'r') GO TO 4
	IF (il != 0) {
 	   WRITE (ttyout,*)'No input, try again.'		
	   GO TO 4	
	}
	IF (x < 0 | x > 1536) {
	   WRITE (ttyout,*)'ERROR: record length must range from 0-1536 bytes'
	   GO TO 4	
	}
	reclen = x
#	filtyp(k,3)=reclen

5	WRITE (ttyout,*)'What is the length of the record header in bytes?'
	CALL crtin
	i=1
	CALL wjfren (i,x,il)
	IF (il == ihe | il == ihx) {  # exit if an e or x encountered
	   RETURN
	   STOP
	}
	IF (il == 'r') GO TO 5
	IF (il != 0) {
 	   WRITE (ttyout,*)'No input, try again.'		
	   GO TO 5
	}
	IF (x < 0 | x > 1536) { WRITE (ttyout,*)'Invalid input';GO TO 5}
	rechdr=x
# 	filtyp(k,4)=rechdr

6	WRITE (ttyout,*)'What is the DN offset?'
	CALL crtin
	i=1
	CALL wjfren (i,x,il)
	IF (il == ihe | il == ihx) {  # exit if an e or x encountered
	   RETURN
	   STOP
	}
	IF (il == 'r') GO TO 6
	IF (il != 0) {
 	   WRITE (ttyout,*)'No input, try again.'		
	   GO TO 6
	}
	IF (x < 0) { WRITE (ttyout,*)'Invalid input';GO TO 6}
	dnoff=x
#	filtyp(k,5)=dnoff

7	WRITE (ttyout,*)'What is the DN scale?'
	CALL crtin
	i=1
	CALL wjfren (i,x,il)
	IF (il == ihe | il == ihx) {  # exit if an e or x encountered
	   RETURN
	   STOP
	}
	IF (il == 'r') GO TO 7
	IF (il != 0) {
 	   WRITE (ttyout,*)'No input, try again.'		
	   GO TO 7
	}
	IF (x < 0) { WRITE (ttyout,*)'Invalid input';GO TO 7}
	scale=x
#	filtyp(k,6)=scale

#  Print visual aids for user
8	WRITE (ttyout,*)'                     ......................'
	WRITE (ttyout,*)'                   .                    . .'
	WRITE (ttyout,*)'  ................                    .   .'
	WRITE (ttyout,*)'.....................................     .'
	WRITE (ttyout,*)'. File header  .                    .     .'
	WRITE (ttyout,*)'................                    .     .'
	WRITE (ttyout,*)'               .                    .     .'
	WRITE (ttyout,*)'            x  .       data         .     .'
	WRITE (ttyout,*)'               .                    .     . '
	WRITE (ttyout,*)'               .                    .   .'
	WRITE (ttyout,*)'               .                    . . z'
	WRITE (ttyout,*)'               ......................'
	WRITE (ttyout,*)'                          y'
	WRITE (ttyout,*)


#  Prompt for dimensions of the data cube
9	WRITE (ttyout,*)'Please enter the dimension lengths of the input array'
	WRITE (ttyout,*)'which can be either 2 or 3 -dimensional (x,y,z):'
	READ (ttyin,*)dx,dy,dz
#	filtyp(k,7)=dx
#	filtyp(k,8)=dy
#	filtyp(k,9)=dz

10	WRITE (ttyout,*)'Choose the data type for the file:'
	WRITE (ttyout,*)'----------------------------------'
	WRITE (ttyout,*)
	WRITE (ttyout,*)'1   Integer*2 (half-word) '
	WRITE (ttyout,*)'2   Integer*4 (full-word) - specpr normal'
	WRITE (ttyout,*)'3   Real*4 (full-word)'
	READ (ttyin,*)type
#	filtyp(k,10)=type

	IF (type == 1)  {
	  dattyp='Integer*2'
	}
	ELSE IF (type == 2)  {
	  dattyp='Integer*4'
	}
	ELSE IF (type == 3)  {
	  dattyp='Real*4'
	}
	ELSE  {
	   GO TO 1
	}

#11	WRITE (ttyout,*)'Is the data file specpr normal?'
#	READ (ttyin,*)ans

#  Test to see if the user input the dimensions correctly
	WRITE (ttyout,*)'Your input is as follows:'
	WRITE (ttyout,*)'---------------------------------------'
	WRITE (ttyout,*)infile
	WRITE (ttyout,*)'                 Format = ',chorg
	WRITE (ttyout,*)'              Data type = ',dattyp
	WRITE (ttyout,*)'          Record length = ',reclen
	WRITE (ttyout,*)'     File Header length = ',filhdr
	WRITE (ttyout,*)'   Record Header length = ',rechdr 
	WRITE (ttyout,*)'            x-dimension = ',dx
	WRITE (ttyout,*)'            y-dimension = ',dy
	WRITE (ttyout,*)'            z-dimension = ',dz
	WRITE (ttyout,*)'              DN offset = ',dnoff
	WRITE (ttyout,*)'               DN scale = ',scale
	WRITE (ttyout,*)'---------------------------------------'
	WRITE (ttyout,*)
	WRITE (ttyout,*)

     	WRITE (ttyout,*)'Are these values correct?'
	READ (ttyin,20)ans
20      FORMAT (A3)
	IF (ans == 'Y' | ans == 'y') { 
 	   CONTINUE
	} 
	ELSE  {
	   GO TO 1
	}

#  Open file to be read and check for file opening errors
	OPEN (UNIT=in,FILE=infile,RECL=reclen,ACCESS='DIRECT',STATUS='OLD',
             IOSTAT=ioerr)
	IF (ioerr < 0)  {
	   WRITE (ttyout,*)'Error ',ioerr,' in OPEN statement - MAIN'
	   GO TO 1
	}
	ELSE IF (ioerr > 0)  {
	   WRITE (ttyout,*)'EOF encountered in OPEN statement - MAIN'
	   GO TO 1
	}
	ELSE   {
	   DO i=1,10  {
	      WRITE (ttyout,*)
	   }
	   WRITE (ttyout,*)'OPEN successful.'
	}

#  Test for dimensional organization of array
	IF (dx <= 0) {
   	   WRITE (ttyout,*)'Invalid input for x dimension.'
           GO TO 1
	}
	ELSE IF (dy < 0)  {
   	   WRITE (ttyout,*)'Invalid input for y dimension.'
           GO TO 1
	}
	ELSE IF (dz < 0)  {
   	   WRITE (ttyout,*)'Invalid input for z dimension.'
           GO TO 1
	}
	ELSE  {
	   #  Jump to visual aids routine
	   CALL CUBE

           #  Direct program to correct subroutine
	   IF (orgniz == 1)  {
	      CALL BIL (chbuff,i2buff,i4buff,r4buff)
	   }
	   ELSE IF (orgniz == 2)  {
	      CALL BIP (chbuff,i2buff,i4buff,r4buff)
	   }
	   ELSE {
	      CALL BSQ (chbuff,i2buff,i4buff,r4buff)
 	   }
	}

#  End program
	STOP
	END
