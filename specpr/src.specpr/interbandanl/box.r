subroutine box(x,y,side)
# 
#
#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa and R. Clark
#ccc  language:
#ccc
#ccc  short description:
# This routine draws a box around a given point with sides of length
# side, all parameters are in hp graphics coordinates
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

	integer*4 x,y,side

	call movabs(x-side,y-side)
	call drwabs(x-side,y+side)
	call drwabs(x+side,y+side)
	call drwabs(x+side,y-side)
	call drwabs(x-side,y-side)

return
end

