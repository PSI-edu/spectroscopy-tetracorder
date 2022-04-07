	subroutine prochk
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine verifies that the protection
#ccc         on the file is consistent with the actual file
#ccc         size. If it is not consistent it prints a warning
#ccc         message
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin
#ccc  argument list description:
#ccc        arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#                                                                      #
#      this routine verifies that the protection on a file             #
#      is consistent with the actual file fsize. If it is not          #
#      consistent it prints a warning message.                         #
#                                                                      #
########################################################################

	include "../common/lblvol"
	include "../common/lblprt"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/alphabet"
	include "../common/ioftyp"

#RED
	integer*4 lnb        # function lnb

	character*9     anull
	character*80    dummy(6)
	character*9     tape

	integer*4 prot(6),map(6),map2(6)
	integer*4 fsize,filsiz

	equivalence (prot(1),iprtu),(dummy,ivfl)

	data anull /NULL/
	data tape /DTAPE/
	data map  /4,5,3,6,1,2/
	data map2 /4,5,3,6,1,2/

	do i=1,6 {
		if (prot(map(i))==-1) next
		if (dummy(i)(1:10)==anull) next
		if (dummy(i)(1:9)==tape)  next
		filsiz = fsize(dummy(i))
		if (map(i) < 6) {          # don't do 6 because that is starpack
			itype = filtyp(1,map(i))  # set file type for
							# nonspecpr files
		} else {
			itype = 0  # this case is specpr starpack file
		}
		if (itype == 0) {            # specpr normal files
			if (mod(filsiz,512)!=0) {
				write(ttyout,50) dummy(i)(1:lnb(dummy(i)))
				call crtin
				return
			} else if (i==4 & filsiz!=0)
				filsiz = (filsiz)/77824    # starpack file
			else if (filsiz!=0)
				filsiz = (filsiz-1536)/1536    # data file
			if (filsiz != iabs(prot(map(i)))) {
				newpro = filsiz
				newprr = -1*(iabs(filsiz))
				itmp=index(dummy(i)(1:lnb(dummy(i))),'/')
				if (newprr > -2) newprr = -2
				if (prot(map(i)) < 0) newpro = -1 * iabs(newpro)
				if (itmp > 0) newpro = -1 * iabs(newpro)

				write (ttyout,100) dummy(i)(1:lnb(dummy(i))),
							filsiz,prot(map(i)),
							newpro,newprr
				call crtin
				ij = 1
				call wjfren (ij,x,il)
				if (il == ihc) prot(map(i)) = newpro
				if (il == ihr) prot(map(i)) = newprr
			}
		} else {
			if (map(i) < 6) { # not starpack, not normal specpr file
				irecsz = filtyp(3,map(i))
				if (irecsz > 0) {    # valid record size
					filsiz = filsiz/filtyp(3,map(i))
					if (filsiz != iabs(prot(map(i))) ||
							prot(map(i)) > -2) {
						newpro = filsiz
						newprr = -1*(iabs(filsiz))
						write (ttyout,
						101) dummy(i)(1:lnb(dummy(i))),
							filsiz,prot(map(i)),
							newprr
						call crtin
						prot(map(i)) = newprr
					}
				}
			}
		}
	}
	return

50      format(' WARNING -- ',a,' is NOT a NORMAL specpr file!!',/,
               '            and it is NOT assigned as a SPECIAL 3D file',/,
	       ' press RETURN TO CONTINUE')
100     format(' WARNING -- ',a,' contains ',i8,
	       ' records but protection is ',i8,/,
	       ' type  c  to CHANGE PROTECTION TO',i8,/,
               '       r  to CHANGE PROTECTION TO READ ONLY:',i8,/,
               ' or press return to CONTINUE')
101     format(' WARNING -- ',a,' contains ',i8,
	       ' records but protection is ',i8,/,
               ' 3D special files can only be READ ONLY at this time',/,
               ' type return to CHANGE PROTECTION TO READ ONLY:',i8)
	end
