	subroutine wristr(ier,ifiln)
	implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc                   this subroutine writes the starpack record
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    devsta,drite,erored,newsta,crtin
#ccc  argument list description:
#ccc       argument: ier,ifiln
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lblprt"
	include "../common/lundefs"
	include "../common/dscrch"
	include "../common/label1"

	integer*4 ibuf0(6),ibuf1(60),ibuf2(75),ibuf3(148),ibuf4(SPMAXCHAN)
	integer*4 idummy
	equivalence (ibuf0,jdateb),(ibuf1,revs1),(ibuf2,ititl1),(ibuf3,mhista)
	equivalence (ibuf4,datsc4)
	
	call devsta(slun,ista,0,iprt)
	ier = 0
	if (ista==-4) ier = 4
	else {
		if (ifiln<1) ifiln = 1
		if (ifiln>50) write(ttyout,30)
		else {
			if (iprt!=-1) {
				if (iprt>=(-1))
					if (ifiln==iprt+1)
						go to 10
				write(ttyout,40)
				go to 20
				}
10			ier = 0
#
# a starpack now takes 4 virtual records (each is 19456 bytes long)
# header stuff is first, dataa 2nd, datab 3rd, datac 4th.
# This routine has been changed so that it writes a single 
# 77824 byte starpack real record (R.H.B. 8/1/90)
#

#
# first put in header stuff: ibuf0(6),ibuf1(60),ibuf2(75),ibuf3(148)
#
			do i = 1, 75 {
				ibuf4(i) = ibuf2(i)
			}
			do i = 1, 148 {
				ibuf4(i+75) = ibuf3(i)
			}
			do i = 1, 60 {
				ibuf4(i+75+148) = ibuf1(i)
			}
			do i = 1, 6 {
				ibuf4(i+60+75+148) = ibuf0(i)
			}
			do i = 290, SPMAXCHAN {
				ibuf4(i) = 0
			}

			write(slun,rec = ifiln,iostat=idummy)ibuf4,dataa,datab,
									datac
			call ertyp('wristr',slun,idummy)

			ier = idummy
			call newsta(slun,11,ifiln)
			if (iprts>=0)
				iprts = iprts+1
			return
			}
20	 	ier = 4
		call crtin
		ifiln = 1
		}
print*,'return from wristr,ifiln,ier=',ifiln,ier
	return
30  format(' ERROR: starpack request greater than 50',/)
40  format(' ERROR: file write protect error',/)
	end
