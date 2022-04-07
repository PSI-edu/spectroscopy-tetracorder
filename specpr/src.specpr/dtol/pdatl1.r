subroutine pdatl1 (ia,ib,ic,id,itf,ih,im,irec,lpline,itw,inm)
#	Ratfor
#*************** subroutine pdatl1 *****************************
#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#arguments:
#		ia=1: 	print title,revs,int. time,ctb
#			dateb,airmas,cond. history
#		ib=1: 	print all of header except man.history	
#		ic=1:	print manual history 
#		id=1:	print data
#		itf=1:	print text 
#		itec=1:
#		ih=1:	print history
#		lpline:	input/output current line on outpu page


	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first


	include "../common/lundefs"
	include "../common/label1"

	character*8 cxiang,cxeang,cphase,inm
	integer*4 yy,mm,dd
	real*4 xiangl,xeangl,phase
	character*11 xptime
	character*10 xpdate

#  print: rec,title,channels,revs,btime,bdate,iangle,eangle,temp
#

	intsph=2000000000
	call todms(iscta,24000,ihr,ihm,ss)
	call todms(isctb,24000,ihsr,ihsm,sss)
	call frjuld(yy,mm,dd,jdateb)

	xiangl=dble(siangl)/21600000.0d0
	xeangl=dble(seangl)/21600000.0d0
	phase=dble(sphase)/5400000.0d0

	if (siangl==intsph) {
		cxiang=" int sph"
	}else {
		write (cxiang,100) xiangl
	}
	if (seangl==intsph) {
		cxeang=" int sph"
	}else {
		write (cxeang,100) xeangl
	}

	
	if (sphase==intsph) {
		cphase=" int sph"
	}else {
		write (cphase,105) phase
	}
	
	write(xptime,160)ihr,ihm,ss
	write(xpdate,170)mm,dd,yy
	call zeroin(xptime)
	call zeroin(xpdate)

	if (ia==1) {
		call plpage(lpline,inm)
		if (lpline.le.5|| itw==1) {
			write(lstlun,150)
			lpline=lpline+2
		}
		write (lstlun,200) irecno,ititl,itchan,
	 	revs,xptime,xpdate,cxiang,cxeang,cphase,irwav
		lpline=lpline+1
		call plpage(lpline,inm)
		if (ih==1) {
			write(lstlun,201) ihist
			lpline=lpline+1
		}
		call plpage(lpline,inm)
			if (ihist==' ') goto 5
			if (ihist(1:4)!='   r') {
				if (ihist(1:4)=='avg:'|
				    ihist(1:4)=='ave:'|
				    ihist(1:4)=='sum:'|
				    ihist(1:4)=='f8: '|
				    ihist(1:4)=='f11:'|
				    ihist(1:4)==' gfi') { 
				     if (mhist(1:74)!=' ') {
				      write(lstlun,120) mhist(1:74)
				      write(lstlun,120)mhist(74:148)
				      write(lstlun,120)mhist(149:222)
			              lpline=lpline+3 
				      call plpage(lpline,inm)	
				      }	
				     write(lstlun,10)
				     lpline=lpline+1
				     goto 50
    			             }      #end of if special ihist

#				write(lstlun,10)
#				lpline=lpline+1
			}                   #end ihist<>'     r' 
5		call plpage(lpline,inm)
		if (lpline.le.5) {
			write(lstlun,150)
			lpline=lpline+2
		}	


		if (im!=1) { 
			if (mhist(1:2)!='**') goto 1000
		}else{
			write(lstlun,27)
			lpline=lpline+1
			for (i=1;i<224;i=i+73) {
				if (mhist(i:i+4)!='     ') {
				   write(lstlun,25) mhist(i:i+73)
				   lpline=lpline+1 
				   call plpage(lpline,inm)
				}
			}
			write(lstlun,27)
			lpline=lpline+1
		}


50		call plpage(lpline,inm)
	}else{
	 if(ib == 1){
              	 call prhead(lpline,xptime,xpdate,ic,inm)
		}
	}
	 if (id == 1) {
		ida=float(itchan)/6.0+.9
		la=-5
		lb=0
		do i= 1,ida {
			la=la+6
			lb=lb+6
			if(lb >= itchan) lb = itchan
			lpline = lpline + 1
			write(lstlun,400)(lc,data(lc),lc= la,lb)
			call plpage(lpline,inm)
		}
		write(lstlun,410)
		lpline = lpline + 1
		call plpage(lpline,inm)
	}
1000	if (lpline > 5) {
		write (lstlun, 420)
		lpline = lpline+1
	}
	call plpage(lpline,inm)
	return
	
8	format(12x,a)
10	format(1x) 
16	format(74(1h.),/,a)
17	format(40x,a)
25	format(8x,a)
27	format(8x,74('-'))
100	format(f8.4)
105	format(f8.3)
120	format(5x,'mhist: ',a)
150 	format('record',14x,'title',25x,'chans',4x,'rev',4x,'time',8x,
	'date',8x,'incid',5x,'emiss',5x,'phase',5x,'waves',/)
160	format(I2,':',I2,':',F5.2)
170	format(I2,'/',I2,'/',I4)
180	format(A11,3x,A10)
190	format(A11,3x,A10)
200 	format(I6,2x,a,1x,I5,1x,I6,2x,a,2x,a,2x,a,2x,a,2x,a,2x,I6)
201	format(8x,a)
203	format(I6,2x,a,1x,I5,1x,a,2x,a,2x,f7.2)
400	format(1x, 6(I3,1x,1pe12.5,4x))
410     format(1x,115('*'))
420	format(1x,'   ')

	end	
	
