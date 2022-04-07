	character*1 function chchar(i)
	implicit integer*4 (i-n)
        integer*4 i

#ccc

#  remove the embedded spaces put in by ihchar(char)
# (little-endian):  ihchar=((256*icharb + icharb)*256 + icharb)*256 +ichar(c)
# (big-endian):     ihchar=((256*ichar(c) + icharb)*256 + icharb)*256 +icharb


# byte swapped machines (little-endian):
#   i = 4 byte integer: sp sp sp ascii()

#DEC		chchar=char(mod(i,256))
#LINUX		chchar=char(mod(i,256))
#INTEL		chchar=char(mod(i,256))

# IEEE byte order machines (big-endian):
#   i = 4 byte integer: ascii() sp sp sp

#  256 cubed = 16777216
#HPUX		chchar=char( int(i/16777216) )
#IA64HPUX	chchar=char( int(i/16777216) )
#SUNOS		chchar=char( int(i/16777216) )
#SOLARIS	chchar=char( int(i/16777216) )
#

                return
                end
