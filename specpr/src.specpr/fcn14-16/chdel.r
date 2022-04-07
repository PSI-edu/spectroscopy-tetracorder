	subroutine chdel(i,x,ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine deletes channels for f14.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    wjfren,dltpts
#ccc  argument list description:
#ccc     arguments: i,x,ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     **************************************
#     *
#     * routine for f14 to delete channels
#     *
#     **************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/lbl3"
	include "../common/alphabet"
	include "../common/label1"
	include "../common/dscrch"

	integer*4 idlt(SPMAXCHAN)
	equivalence (idlt,datsc3)


	iflg = 0
	ii = i
	call wjfren(i,x1,ic1)

#     *** check if want to compress ***
	if (ic1==ihc && x1==0.0) {
		iflg = 1
		ii = i
	}
	if (ic1==ihx) return

#     *** delete points ***
	call dltpts(ii,ipts,idlt,maxchn,ic)
	if (ic==ihe || ic==ihx) return
	if (ipts != 0) {
		if (iflg == 0) {
			do  k = 1,ipts {
				datac(idlt(k)) = -1.23e34
				if (ictrl == ihe) error(idlt(k)) = 0.0
			}

#        *** compress if user specified ***
		} else {
			do  k = 1,ipts {
				datac(idlt(k)) = -9.99e34
			}
			j = 1
			do  k = 1,maxchn {
				if (datac(k) != -9.99e34) {
					dataa(j) = datac(k)
					if (ictrl == ihe) datab(j) = error(k)
					j = j + 1
				}
			}

#           *** pad end of array with zeroes ***
			l = (maxchn+1) - ipts
			do  k = l,maxchn {
				dataa(k) = 0.0
				if (ictrl == ihe) datab(k) = 0.0
			}

#           *** copy array ***
			do  k = 1,maxchn {
				datac(k) = dataa(k)
				if (ictrl == ihe) error(k) = datab(k)
			}
		}
	}
	return
	end
