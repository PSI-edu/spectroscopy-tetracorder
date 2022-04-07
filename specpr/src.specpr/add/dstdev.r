	subroutine dstdev(js,nchans,error2,datav,idltfg,
		idsum,ifels,inumd,deviat)
	implicit integer*4 (i-n)
#cc  name: dstdev
#cc  version date: 07/26/83
#cc  author(s): Roger Clark & Jeff Hoover
#cc  language: RATFOR
#cc
#cc  short description:
#cc
#cc  algorithm description:
#cc  system requirements:
#cc  subroutines called:
#cc  argument list description:
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  NOTES:
#cc
#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#       this routine deletes the data points that are three
#       standard deviations away from the mean
#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/dscrch"

	real*4 error2(nchans),datav(nchans),data(SPMAXCHAN)
	integer*4 idsum(js),ifels(js),inumd(nchans)
	equivalence (data,datsc1)
	integer*4 ier

	write (ttyout,10) deviat
	do i=1, js {
		read(addlun,rec=i+1,iostat=ier)data
		call ertyp('dstdev',idev,ier)
		write (ttyout,400) idsum(i),ifels(i)
# RED Initialize iflag
		iflag=0
		do j=1,nchans {
			deverr=error2(j) * deviat * sqrt(float(inumd(j)))
			tdata = abs(data(j)-datav(j))
			if (tdata>deverr) write (ttyout,500) j,data(j)
			if (tdata>deverr) data(j)=-1.23e34
			if (tdata>deverr) iflag=1
		}
		if (iflag==0) write (ttyout,600)
		write(addlun,rec=i+1,iostat=ier)data
		call ertyp('dstdev',idev,ier)
	}
	idltfg=1
	return

10      format (' ** deleting  outside ',f4.2,'  st. deviations **')
400     format (' file id -> ',a,i4)
500     format ('      channel ',i4,' data ->',e13.6,' ***deleted***')
600     format (' **** no points deleted ****')

	end
