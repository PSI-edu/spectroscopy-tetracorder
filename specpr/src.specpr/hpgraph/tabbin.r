	subroutine tabbin (ix1,iy1,char1,char2)
	implicit integer*4 (i-n)

#ccc  name: tabbin
#ccc  version date: 2/4/86
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: convert plot-10 tektronix to absolute binary
#ccc
#ccc  algorithm description:
#ccc  system requirements: none
#ccc  subroutines called: none
#ccc  argument list description:
#ccc  parameter description: none
#ccc  common description: none
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	character*2 char1, char2


	ix = ix1
	iy = iy1

#	write(6,10) iy, ix
#10      format("y = ",i4," x = ",i4)

	if (ix < 0) ix = 0
	if (iy < 0) iy = 0
	if (ix > 1023) ix = 1023
	if (iy > 760) iy = 760

	ihix = ix / 32
	ilox = ix - 32*ihix
	ihiy = iy / 32
	iloy = iy - 32*ihiy

	char1(1:1) = char(ihiy + 32)
	char1(2:2) = char(iloy + 96)
	char2(1:1) = char(ihix + 32)
	char2(2:2) = char(ilox + 64)

	return
	end
