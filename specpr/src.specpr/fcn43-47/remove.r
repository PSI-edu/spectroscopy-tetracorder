	subroutine remove(x,y,z,n)
	implicit integer*4 (i-n)
	real*4 x(n),y(n),z(n)

#ccc  name: remove
#ccc  version date:  May 27, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description: Divides x by y and puts into z, for n channels
#ccc
#ccc  algorithm description: z(i)=x(i)/y(i)
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
	do i=1,n {
		if (x(i)==-1.23e34 || y(i)==-1.23e34 ||
					abs(y(i)) < 0.1e-25) {
			z(i)= -1.23e34
		} else {
			z(i)=x(i)/y(i)
		}
	}
	return
	end
