#     this program tests the hp graphics package
#      subroutine hptest
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

	include "../common/hptrm"
	icnt=80

	igrlu = 1
	call hreset (0)
1	print *, 'type in the mode'
	read (5,21) i
	call initt (i)
	print *, '# beeps ?'
	read (5,21) i
	call hbell(i)
10	call respag
	print *, ' line type ?'
	read (5,21) i
	call hpline (i)
	call movabs (20,20)
	call drwabs (500,20)
	call drwabs (500,340)
	call drwabs (20,340)
	call drwabs (20,20)
	call movabs (210,180)
	call sb(0)
	print *, 'graphics test'
	call movabs (100,100)
	call drwabs (100,260)
	call drwabs (420,260)
	call drwabs (420,100)
	call drwabs (100,100)
	a=30.
	am=0.5
	do  i= 1,113 {
		x=i
		x=x*4 +20
		y=am*x +30.
		ix= x+0.5
		iy= y+0.5
		if (i.eq.1) call movabs (ix,iy)
		call drwabs (ix,iy)
		call drwabs (ix+3, iy)
		call drwabs (ix-3, iy)
		call movabs (ix,iy+3)
		call drwabs (ix,iy-3)
		call drwabs (ix,iy)
	}
	call movabs (0, 340)
	call sb(0)
	a=1.3
	do  i= 1,  5000
		b=a*a
	go to 1
21	format (i5)
	end
