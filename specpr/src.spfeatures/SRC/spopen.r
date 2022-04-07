	subroutine spopen(afile,last,ier)
	implicit integer*4 (i-q)
	character*20 afile,bfile
	integer*4 zlun,fnb,lnb
	logical*4 ex
	zlun=0


# 	Check for imbedded blanks
	i=fnb(afile)
	j=lnb(afile)
	if (j==0) {
		ier=-3
		return	
	}
	do k=i,j {
		if (afile(k:k)==' ') {
			ier=-4
			return
		}
		bfile=afile(i:j)
	}
#	check to see if it's new
	inquire(file=bfile,exist=ex,iostat=ier)

#	its not, so open and find last record
	if (ex) {
		open (zlun,file=bfile,status='old',iostat=ier,access='direct',
			form='unformatted',recl=128)
		ilen=int(fsize(bfile//char(0)))
		if (mod(ilen,128) != 0) {
			last=int(float(ilen)/128.0)
		} else {
			ier=-5
			return
		}
	} else {
		open (zlun,file=bfile,status='new',iostat=ier,access='direct',
			form='unformatted',recl=128)
		last=0
	}
	return
	end
