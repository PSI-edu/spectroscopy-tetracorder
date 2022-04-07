	subroutine reastr(iflag,iflnu)
	implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc         this subroutine reads a starpack record (record
#ccc         number = iflnu).  return error flag= iflag
#ccc         with no errors, iflag = 0
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          devsta,dread,erored,newsta,crtin
#ccc  argument list description:
#ccc         arguments: iflag,iflnu
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
	include "../common/lundefs"
	include "../common/dscrch"
	include "../common/label1"

	integer*4 ibuf0(6),ibuf1(60),ibuf2(75),ibuf3(148),ibuf4(19456)
	integer*4 idummy
	real*4 datbuf(19456)
	equivalence (ibuf0,jdateb),(ibuf1,revs1),(ibuf2,ititl1),(ibuf3,mhista)
	equivalence (ibuf4,datbuf)

	integer*4 iflag

	iflag = 0

	if (iflnu<1)
		iflnu = 1
	if (iflnu>50)
		write(ttyout,20)
	else {
		call devsta(slun,ista,0,iprt)
		if (ista==(-4)) {
			iflag = 4
			return
		}
		if (iprt!=-1 && iflnu>iabs(iprt)) {
			write(ttyout,30)
			go to 10
		}
#
# a starpack now takes 4 records (each is 19456 bytes long)
# header stuff is first, dataa 2nd, datab 3rd, datac 4th.
# this routine now reads a record 77824 bytes long that contains
# the above 4 records
#

#
# read in record and copy header stuff: ibuf1(60),ibuf2(75),ibuf3(148)
#
		read(slun,rec = iflnu,iostat=idummy)ibuf4
		call ertyp('reastr',slun,idummy)
		do i = 1, 75 {
			ibuf2(i) = ibuf4(i)
		}
		do i = 1, 148 {
			ibuf3(i) = ibuf4(i+75)
		}
		do i = 1, 60 {
			ibuf1(i) = ibuf4(i+75+148)
		}
		do i = 1, 6 {
			ibuf0(i) = ibuf4(i+60+75+148)
		}
#
# now copy data arrays
#                       NOTE: this needs rewriting for > 4852 chans
#

		do i = 1, 4864 {        # NOTE: this needs rewriting for > 4852 chans
			dataa(i) = datbuf(i + 4864)   # NOTE: this needs rewriting for > 4852 chans
			datab(i) = datbuf(i + 9728)   # NOTE: this needs rewriting for > 4852 chans
			datac(i) = datbuf(i + 14592)   # NOTE: this needs rewriting for > 4852 chans
		}

		call newsta(slun,11,iflnu)
		return
	}
10  iflnu = 1
	call crtin
	iflag = 4
	return
20  format(' ERROR: starpack request greater than 50',/)
30  format(' ERROR: file request greater than protection',/)
	end
