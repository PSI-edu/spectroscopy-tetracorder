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
	character*80 line
	icnt=80

#HPUX	igrlu = 1
	call hreset(0)
1	print *, 'type in graphics mode'
	read (5,21) i
	call initt (i)
	if (i == -1) {
		write(6,*) 'problems initializing graphics'
		write(6,*) 'aborting.'
		stop
	}
	write(line,8888) 'line type ?',char(0)
	call gwrite(line)
	read (5,21) i
	call hbell(-3)
10	call respag
	call hpline (i)
	call movabs (20,20)
	call drwabs (500,20)
	call drwabs (500,370)
	call drwabs (20,370)
	call drwabs (20,20)
	call movabs (221,375)
	call sb(0)
	write(line,8888) 'graphics test', char(0)
	call gwrite(line)
	call movabs (128,99)
	call drwabs (128,291)
	call drwabs (384,291)
	call drwabs (384,99)
	call drwabs (128,99)
	a=30.
	am=0.5
	do  i= 0,127 {
		x=i
		ix=x*4
		iy=x*3
		if (i.eq.0) call movabs (ix,iy)
		call drwabs (ix,iy)
		call drwabs (ix, 390-iy)
		call drwabs (ix+2, 390-iy-2)
		call drwabs (ix+2, iy+2)
	}
	call serase(128,99,384,291)
	do i = 0, 63 {
		ix=128+i*4
		iy=99+i*3
		if (i.eq.0) call movabs (ix,iy)
		call drwabs (ix,iy)
		call drwabs (512-ix, iy)
		call drwabs (512-ix-2, iy+2)
		call drwabs (ix+2, iy+2)
	}
	call movabs(100,10)
	call drwabs(10,100)
	call sb(0)
   write(line,8888) "Point and shoot",char(0)
	call gwrite(line)
   call xcrsrd(ix,iy)
   write(6,*) 'read ',ix,iy
   write(line,8888) "Enter text string",char(0)
	call gwrite(line)
###XWIN   call xcrtin(line)
   write(6,*) line
	call respag
	go to 1
21	format (i5)
8888	format(a,a)
	end
