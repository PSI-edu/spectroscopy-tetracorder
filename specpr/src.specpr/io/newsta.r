	subroutine newsta (idev, ic, ifile)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Fortran
#ccc
#ccc  short description:
#ccc                    This subroutine updates the file position of the
#ccc                    magtapes and the data record number of the last
#ccc                    file accessed on the disk files.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc       arguments: idev, ic,ifile
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	include "../common/lbl7"
	include "../common/labelf"
#c
#c
#c     this routine updates the file position of the mag tapes and
#c     and the data record number of the last file accessed on the
#c     disk files.
#c
	k= 0
	j=1
	if (ic.eq.-1) j=-1
	if (ic.eq.-1) go to 100
	if (ic.eq.14) go to 100
	if (ic.eq.10) k= k- 1
	if (ic.eq.11) k= k+ 1
	if (idev.eq.3) isvcu= ifile+ k
	if (idev.eq.4) iwjcy= ifile+ k
	if (idev.eq.8) isavf= ifile +k
	if (idev.eq.9) iwjf = ifile +k
	if (idev.eq.17) istrf=ifile+ k
	if (idev.eq.7) iwrkf=ifile+ k
	return

100     continue
	if (idev.eq.3) isvcu= j
	if (idev.eq.4) iwjcy= j
	if (idev.eq.8) isavf=j
	if (idev.eq.9) iwjf =j
	if (idev.eq.17)istrf=j
	if (idev.eq.7) iwrkf=j
	return
	end
