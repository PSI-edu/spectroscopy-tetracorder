	subroutine pdata (ia,ib,ic,id,j,lpline)
	implicit integer*4 (i-n)

#ccc  version date: 06/18/85
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine makes line printer listings
#ccc         of specpr files.  Format is oriented to telescopic data
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          plpage,namdev
#ccc  argument list description:
#ccc       arguments: ia,ib,ic,id,j,lpline
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       this routine makes line printer listings of specpr      #
#       files.                                                  #
#                                                               #
#       AUTHOR: R Clark                                         #
#       modified:   04-06-83    JAH   convert to ratfor         #
#       modified:   06/18/85    RNC   convert to new format     #
#	modified:   08/14/87	NSG   fix whats passes		#
#					somebody goofed...	#
#                                                               #
#       arguments:                                              #
#         ia =1     input   print title,revs,int.time,ctb,      #
#                           dateb,airmas,and cond.history       #
#         ib =1     input   print all of header except man.hist #
#         ic =1     input   print manual history                #
#         id =1     input   print data                          #
#         j         input   record number of current record     #
#         lpline    input/output current line on output page    #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"

	character*8     inm
	character*10	ctim1,ctim2,cdat1

	integer*4 yy, mm, dd

	intsph = 2000000000
	iflag  = 0

	airmas=irmas/1000.
	call namdev (idv1,inm)
	if ((id==1) && ((lpline+15+nchans/6)>55)) lpline=60

	if (ia==1) {
		call plpage (lpline, inm)
		if (lpline<=5) write (lstlun, 30)
		if (lpline<=5) lpline= lpline+2

		call todms (isctb, 24000, ihr,ihm,ss)
		call todms (istb, 24000, ihsr, ihsm, sss)
		call frjuld (yy,mm,dd,jdateb)

		write (ctim1, 501) ihr,ihm,ss
		write (ctim2, 501) ihsr,ihsm,sss
		write (cdat1, 502) mm,dd,yy

		do itmp = 1, 10 {
			if (ctim1(itmp:itmp) == " ") ctim1(itmp:itmp) = "0"
			if (ctim2(itmp:itmp) == " ") ctim2(itmp:itmp) = "0"
			if (cdat1(itmp:itmp) == " ") cdat1(itmp:itmp) = "0"
		}
		write (lstlun,7) irecno,ititl,itchan,revs,timint,
					ctim1,ctim2,cdat1,airmas,xnrm
		lpline=lpline+1
		if (ihist(1:4)!='   r') {
			lmi= 0
			if (ihist==' ') goto 11
			lpline=lpline+1
			write (lstlun,8) ihist
			if (ihist(1:4) == 'avg:' ||
			    ihist(1:4) == 'ave:' ||
			    ihist(1:4) == 'sum:' ||
			    ihist(1:4) == 'f8: ' ||
			    ihist(1:4) == 'f11:' ||
			    ihist(1:4) == ' gfi') {
					iflag = 0
					if (mhist(1:74) != ' ') {
                			        lpline = lpline + 3
				                call plpage(lpline,inm)
						write(lstlun,16) mhist(1:74)
						iflag = 1
					}
			        write (lstlun,10)
			        lpline=lpline+1
					goto 15
			}
			write (lstlun,10)
			lpline=lpline+1
		}
11      	call plpage (lpline, inm)
		if (lpline<=5) {
			write (lstlun, 30)
			lpline= lpline+2
		}
		if (mhist(1:2)!='**') go to 9000
		lpline = lpline + 3
		call plpage(lpline,inm)
		write(lstlun,16) mhist(1:74)
#
# the flag nonsense is to preserve the way the routine
# used to be while adding the fact that even if there
# is a ave: etc in the hist, if there is no mhist, it
# should not be printed.  It was before, it isn't now! timj
#
		iflag = iflag + 1
15      	for (i=75; i<224; i=i+74) {
			if(mhist(i:i+1)=='  ') break
			iflag = iflag + 1
			write(lstlun,17)mhist(i:i+73)
			lpline=lpline + 1
		}
		call plpage(lpline,inm)
		if (iflag > 0) write(lstlun,2345)
	}
	if (ib==1) {
		call plpage (lpline, inm)
		write (lstlun,205) filno,ititl,ctb(1),ctb(2),ctb(3),
			dateb(1),dateb(2),dateb(3),revs,airmas
		write (lstlun,210) ira,idec,cta(1),cta(2),cta(3),
			datea(1),datea(2),datea(3),
			stb(1),stb(2),stb(3),
			sta(1),sta(2),sta(3)
		write (lstlun,215) ifut(1),ifut(2),xnrm,timint,nruns,iwtrns,scatim,
			ihist,itimch
		lpline= lpline + 6
		if (ic+id==0) write (lstlun,10)
		if (ic+id==0) lpline= lpline +1
		call plpage (lpline, inm)
		if (ic==1 || mhist(1:2)=='**')   write (lstlun,305) mhist(1:74),
			mhist(75:148),mhist(149:222),mhist(223:296)
		write (lstlun,310)
		if (ic==1) lpline= lpline +6
		if (id==0) write (lstlun,10)
		if (id==0) lpline= lpline +2
		call plpage (lpline, inm)
		if (id!=1) goto 9000

		ida= float(nchans)/6.0 +0.9
		la=-5
		lb= 0
		do i=1,ida {
			la= la +6
			lb= lb +6
			if (lb.gt.nchans) lb= nchans
			lpline=lpline+1
			write (lstlun,420) (lc,data(lc),lc=la,lb)
		}
		write (lstlun,310)
		lpline=lpline+1
	}
9000    continue
	return
7       format (i6,2x,a,2x,i5,2x,i6, f8.2,3x, a,
		2x, a, 2x, a, 2x, f6.2, 2x, 1pe11.4)
8       format (12x, a)
10      format (1x)
16      format(40x,74(1h.),/,40x,a)
17      format(40x,a)
18      format(1x,"		")
30      format (1x, 'rec. no.', 10x, 'title', 25x, 'chans', 2x,'revs', 2x,
		'int time', 2x, 'cvl/u time', 3x, 'sid time', 5x, 'date', 5x,
		'airmass', 2x, 'normalization', /)
205     format (1x, i6,5x, a,5x, 'ctb=',2(a,':'),a,3x, 'date b=',
		2(a,'/'),a,3x, 'revs=', i5, 3x, 'airmass=',f7.3, /, 12x,
		119(1h-))
210     format (12x, 'ra=',3i3,3x, 'dec=',3i3,3x, 'cta=',2(a,':'),a,
		3x, 'date a=',2(a,'/'),a,3x, 'sid time b=',2(a,':'),a,
		3x, 'sid time a=',2(a,':'),a)
215     format(12x,'normalization (',i3,',',i3,')=', 1pe12.5, 2x,
		'integration ',
		'time=',0pf8.2,2x, '# runs=',i6, 2x, 'weighted runs=', i6,
		2x, 'scan time=',f7.2,/, 12x, 119(1h-), /, 12x,
		'history=',a, 5x, 'integration time per half chop=',i6,
		' msec')
305     format (2x, 'manual history:',/, 3(10x, a,/), 10x, a)
310     format (1x, 130(1h*))
420     format (1x, 6(i3, 1x, 1pe12.5, 4x))
501	format (i2,':',i2,':',f4.1)
502	format (i2,'/',i2,'/',i4)
2345    format(40x,74(1h-),/)
	end
