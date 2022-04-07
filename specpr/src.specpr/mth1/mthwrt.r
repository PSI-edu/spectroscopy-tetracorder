    subroutine mthwrt(igo,idev,ifile,iprodp)
	implicit integer*4(i-n)

#cc  version date: 06/01/83
#cc  author(s): Roger Clark & Jeff Hoover
#cc  language: Ratfor
#cc
#cc  short description:
#cc                   This subroutine decodes the file id and number of
#cc                   where to write the results
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    crtin,wjfren,devlun,devsta
#cc  argument list description:
#cc          arguments: igo,idev,ifile,iprodp
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/lbl6"
	include "../common/labelf"
	include "../common/lblprt"
	include "../common/alphabet"
	include "../common/lundefs"


#
#     this routine decodes the file id and number of where to write
#     the results.
#
	write(ttyout,40)iprtv,iprtw,iprtd,iprtu,iprty
	if (iprodp==1) {
		iopcon = 'v'
		write(ttyout,50)iopcon
		go to 10
	}
repeat {
	call crtin
	10  idev = 0
	igo = 0
	j = 1
	call wjfren(j,x,il)
#if (il==ihx)
#	go to 30
	if (il==ihe || il == ihx)
		break 1
	call devlun(4,il,idev)
	if (idev!=0) {
		idv1 = il
		call wjfren(j,x,ib)
		if (ib==0) {
			ifile = x
			ifl1 = ifile
			if (ifile>=0) {
				ifl1 = ifile
				call devsta(idev,ista,1,iprt)
				if (ista<=0) {
					write(ttyout,90)
					go to 20
					}
				else if (iprt!=-1||ifile>0) {
					if (iprt==(-1))
						return
					if (iprt<=(-2)) {
						write(ttyout,70)
						go to 20
						}
					else {
						if (ifile==0) {
							ifile = iprt+1
							ifl1 = ifile
						}
						if (ifile==iprt+1) {
							ifl1 = ifile
							return
						}
						write(ttyout,100)
						go to 20
						}
					}
				}
			write(ttyout,80)
			}
		}
	20  write(ttyout,60)
	iprodp = 0
	}
igo = 110
return
30  igo = 1101
return
40  format(1x,'OPERATION COMPLETED.  to WRITE results ',
		'type in the ','file id and record #',/,1x,
	'no file # on a protected ',
	'device defaults to the protected+1 file.',
	/,1x,'FILE PROTECTION: v',i7,',   w',i7,',   d',i7,
  ',   u',i7,',  y',i7,/,1x,72('-'),/)
50  format(a)
60  format(1x,'ERROR reenter',/)
70  format(1x,'*** READ ONLY DEVICE ***')
80  format(1x,'*** ILLEGAL FILE NUMBER ***')
90  format(1x,'*** DEVICE OFFLINE ***')
100  format(1x,'*** FILE PROTECTED ***')
end
