	subroutine bytorder (a,iflag)
	implicit integer*4 (i-n)

# reverse byteorder in a 4-byte word: 1234 -> 4321
#
# Note: while variable "a" is an integer, the ordering will work on
# floats too.
#
# iflag: when to swap bytes on a(1): the bit flags
#        = 0 do not flip bytes on a(1)
#        = 1 flip bytes on a(1) before others (for read)
#        = 2 flip bytes on a(1) after  others (for write)
#
# Roger N. Clark  02/18/2001
#
# Randall Dailey 03/01/2004
#	Minor changes and some bug fixes - see RED comments

	integer*2       chkbit, ibit, ibit0, ibit1

	integer*4 a(384), iflag, itmp

	include "../common/iomap1"

	itmp=0

	if (iflag == 1) call swab4order(a(1))  # swap byte order on bitflags

	ibit0 = 0
	ibit1 = 1
	# RED - changed chkbit(icflag,ibit1) to chkbit(a(1),ibit1 below for consistency
        if (chkbit(a(1),ibit0) == 0 & chkbit(a(1),ibit1) == 1) {     # 1st text data record

		# RED changed to reference 14,15 instead of 13,14
		call swab4order(a(14))  # swap byte order on text pointer
		call swab4order(a(15))  # swap byte order on text size
                #write (*,*) 'DEBUG: byteorder is 1st text record'

	# RED - changed chkbit(icflag,ibit1) to chkbit(a(1),ibit1 below for consistency
        } else  if (chkbit(a(1),ibit0) == 1 & chkbit(a(1),ibit1) == 1) {  # contin text data record

		itmp =0  # do something in the block but really nothing to do.
                #write (*,*) 'DEBUG: byteorder is continuation text record'

	# RED - changed chkbit(icflag,ibit1) to chkbit(a(1),ibit1 below for consistency
        } else  if (chkbit(a(1),ibit0) == 0 & chkbit(a(1),ibit1) == 0) {  # 1st data record

                #write (*,*) 'DEBUG: byteorder is 1st data record'

		# loop number determined by hand from include: iomap1

		do i = 14, 29 {
			call swab4order(a(i))
		}
		# RED changed from 117 to 119
		do i = 119, 384 {
			call swab4order(a(i))
		}

	# RED - changed chkbit(icflag,ibit1) to chkbit(a(1),ibit1 below for consistency
        } else  if (chkbit(a(1),ibit0) == 1 & chkbit(a(1),ibit1) == 0) {  # 1st data record

                #write (*,*) 'DEBUG: byteorder is continuation data  record'
		do i = 2, 384 {
			call swab4order(a(i))
		}

        } 

	if (iflag == 2) call swab4order(a(1))  # swap byte order on bitflags

	return
	end
