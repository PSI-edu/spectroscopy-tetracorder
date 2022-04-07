	subroutine tname(str,length,time)
	implicit integer*4 (i-n)
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#################################################################
#       this routine creates a unique file name using str       #
#       as a template. All trailing x's are replaced by a       #
#       digits from the time 'time'.                            #
#       length is a currently unused argument.                  #
#################################################################

#RED
	integer*4 lnb     # function lnb

	character*(*)   str
	character*12    cct
	integer*4		time

	j = lnb(str)
	for (i=j; i>0 & str(i:i)=='x'; i=i-1)
		;
	write(cct,'(i12)') time
	write(str(i+1:j),'(a)') cct(4:12)

	return
	end
