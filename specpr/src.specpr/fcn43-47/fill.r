	subroutine fill(z,a,b,n,o)
	implicit integer*4 (i-q)
	integer*4 n,i,j,k,z(n)
	real*4 o(n),b(n),a(n)
#ccc  name: fill 
#ccc  version date: May 27, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description: Fills in values between peaks by linear interpolation
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc				z == idat array
#ccc				a == x-data array
#ccc				b == y-data array
#ccc				o == output array
#ccc				n == number of channels
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#	initialize
	do i=1,n {
		if (b(i)!=-1.23e34) o(i)=0.0
		else o(i)=-1.23e34
	}
	i=1
	if (n<4) {
		do l=1,n {o(l)=b(l)} 
		return
	}
#	find two idat-1's
50	if (z(i)==1) {
		if (i<n) {
			x1=a(i)
			y1=b(i)
			if (x1==-1.23e34 || y1==-1.23e34) { 
				z(i)=0
				if (i<n) {
					i=i+1
					go to 50	
				}
			}
		} else {
			return
		}
	} else {
		if (i<n) {
			i=i+1
			go to 50
		} else {
			return
		}
	}
	j=i+1
51	if (z(j)==1) {
		x2=a(j)
		y2=b(j)
			if (x2==-1.23e34 || y2==-1.23e34) { 
				z(j)=0
				if (j<n) {
					j=j+1
					go to 51	
				}
			}
	} else {
		if (j<n) {
			j=j+1
			go to 51
		} else {
			x2=a(j)
			y2=b(j)
		}
	}
#	fill in the remaining values
	do k=i,j {
		if (a(k)==-1.23e34) b(k)=-1.23e34
		if (b(k)==-1.23e34) {
			o(k)=-1.23e34
			next
		}
		o(k)=((y2-y1)/(x2-x1))*(a(k)-x1)+y1
	}
	i=i+1
	go to 50
	end
