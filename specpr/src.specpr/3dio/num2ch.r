# SCCS ID: %Z% %W% %G%
#__________________________________________________________________
#   TITLE:      INTERPRET NUMBER INTO ASCII CHARACTER              |
#   PROGRAMMER: Barry J. Middlebrook                               |
#__________________________________________________________________|
#   DESCRIPTION:                                                   |
#               This is a routine to interpret numbers into ASCII  |
#               characters.  The input must be an integer.         |
#__________________________________________________________________|
#   VARIABLES:                                                     |
#             num       - input integer for interpretation         |
#             len       - length of character string (out)         |
#             lim       - limit for number of digits interpreted   |
#             powr      - power of ten for dividing                |
#             tmp(i)    - working array for interpretation routine |
#             pos       - position in character array              |
#             size       - number of digits in input number        |
#             nztst     - non-zero test to detect first non-zero   |
#                       digit                                      |
#             chr(i)    - character array for interpreted digits   |
#__________________________________________________________________|

	subroutine num2ch (num,size,chr)

#  Set variable type
	implicit integer*4 (i-m)
	integer*4	num,len,powr,tmp(0:7),pos,nztst,lim,size
	parameter	(lim=6)
	character	chr*10

#  Set variables
	powr=10**(lim)
	tmp(lim+1)=num
	size=0
	pos=0
	nztst=0

#  Interpret number
	do i=lim,0,-1  {
	   tmp(i)=tmp(i+1)/powr
	  
	   if (nztst == 1 | tmp(i) != 0)  {
	      nztst=1
	      pos=pos+1
	      chr(pos:pos)=char(tmp(i)+48)
	   }
	   else if (i == 0 & tmp(i) == 0) {
	      size=1
	      pos=1
	      chr(pos:pos)='0'
	      return
	   }
	   else  { continue }

	   tmp(i)=tmp(i+1)-tmp(i)*powr
	   powr=powr/10
	}
	size=pos

#  End program
	return
	end
