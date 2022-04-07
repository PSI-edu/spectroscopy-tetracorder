      subroutine vaw (ix, iy)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine calls the hp subroutine for drawing
#ccc         a vector.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          drwabs
#ccc  argument list description:
#ccc    arguments: ix,iy
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#
#     this routine calls the hp subroutine for drawing a vector.
#
      x= ix
      y= iy
      x=x* 0.498047+ 0.5
      ixa= x
      y=y* 0.460256+ 0.5
      iya= y
      call drwabs (ixa, iya)
      return
      end
