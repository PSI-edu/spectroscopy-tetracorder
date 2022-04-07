subroutine matinv(n,a,b,msignl)
implicit integer*4 (i-n)
dimension  a(66,66),b(66)
dimension pivot(66)
dimension ipivot(66),index(66,2)
#do i = 1,n
#do j = 1,n
#	write(33,30)a(j,i)
#
#     initialization
determ = 1.
do j = 1,n
	ipivot(j) = 0
do i = 1,n {
#     search for pivot element
	amax = 0.0
	do j = 1,n
		if (ipivot(j)!=1)
			do k = 1,n
				if (ipivot(k)<1) {
					if (abs(amax)<abs(a(j,k))) {
						irow = j
						icolum = k
						amax = a(j,k)
						}
					}
				else if (ipivot(k)!=1)
					go to 20
	ipivot(icolum) = ipivot(icolum)+1
#
#     interchange rows to put pivot element on diagonal
#
	if (irow!=icolum) {
		determ = -determ
		do l = 1,n {
			swap = a(irow,l)
			a(irow,l) = a(icolum,l)
			a(icolum,l) = swap
			}
		swap = b(irow)
		b(irow) = b(icolum)
		b(icolum) = swap
		}
	index(i,1) = irow
	index(i,2) = icolum
	pivot(i) = a(icolum,icolum)
	if (pivot(i)==0)
		go to 10
	repeat {
		det = determ*pivot(i)
		if (det!=0)
			break 1
		msignl = 0
		determ = determ*1.0
		}
	determ = det
	a(icolum,icolum) = 1.0
	do l = 1,n
		a(icolum,l) = a(icolum,l)/pivot(i)
	b(icolum) = b(icolum)/pivot(i)
#
#     reduce non-pivot rows
#
	do l1 = 1,n
		if (l1!=icolum) {
			t = a(l1,icolum)
			a(l1,icolum) = 0.0
			do l = 1,n
				a(l1,l) = a(l1,l)-a(icolum,l)*t
			b(l1) = b(l1)-b(icolum)*t
			}
	}
#
#     interchange columns
do i = 1,n {
	l = n+1-i
	if (index(l,1)!=index(l,2)) {
		jrow = index(l,1)
		jcolum = index(l,2)
		do k = 1,n {
			swap = a(k,jrow)
			a(k,jrow) = a(k,jcolum)
			a(k,jcolum) = swap
			}
		}
	}
msignl = -1
return
#    singular matrix
10  msignl = 1
return
#     mixed dimensions
20  msignl = 2
return
#30  format(e15 .6)
end



