	subroutine dpfix(a,b,c,n)
	implicit integer*4 (i-q)
	real*4 a(n),b(n),c(n),x1,x2,y1,y2
#ccc  name: dpfix 
#ccc  version date: May 27, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description: Fixes deleted points by interpolation
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc				a = x-array (wavelengths)
#ccc				b = y-array (data)
#ccc				c = z-array (output)
#ccc				n = number of channels
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
		if (b(i)==-1.23e34) c(i)=-1.23e34
	}
	i=0
#	check for deleted points at front
10	if (c(1)==-1.23e34) {
		j=2
11		if (c(j)!=-1.23e34) {
			if (c(j)==1.0) {
				c(1)=1.0
				go to 20
			}
			k=j+1
12			if (c(k)!=-1.23e34) {
				y1=c(j)
				y2=c(k)
				x1=a(j)
				x2=a(k)
				c(1)=((y2-y1)/(x2-x1))*(a(1)-x1)+y1
				go to 20
			} else {
				if (k<n) {
					k=k+1
					go to 12
				} else return
			}
		} else {
			if (j<n) {
				j=j+1
				go to 11
			}
		}
	}
#	check for deleted points at end
20	if (c(n)==-1.23e34) {
		j=n 
21		if (c(j)!=-1.23e34) {
			if (c(j)==1.0) {
				c(n)=1.0
				go to 30
			}
			k=j-1
22			if (c(k)!=-1.23e34) {
				y1=c(j)
				y2=c(k)
				x1=a(j)
				x2=a(k)
				c(n)=((y2-y1)/(x2-x1))*(a(n)-x1)+y1
				go to 30
			} else {
				if (k>1) {
					k=k-1
					go to 22
				} else return
			}
		} else {
			if (j>1) {
				j=j-1
				go to 21
			}
		}
	}
#	check for deleted points in the middle
30	do i=1,n {
		if (c(i)==-1.23e34) go to 39
	}
		return
39	j=i
40	if (c(j)!=-1.23e34) {
		x1=a(j)
		y1=c(j)
		k=i
50		if (c(k)!=-1.23e34) {
			x2=a(k)
			y2=c(k)
			c(i)=((y2-y1)/(x2-x1))*(a(i)-x1)+y1
			go to 30
		} else {
			if (k<n) {
				k=k+1
				go to 50
			}
		}
	} else {
		if (j>1) {
			j=j-1
			go to 40
		}
	}
	end
