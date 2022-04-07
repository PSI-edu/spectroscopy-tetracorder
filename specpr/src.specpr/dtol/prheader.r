subroutine prhead(lpline,xptime,xpdate,ic,inm)

#
#
#
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/label1"
	real*4 ss,sss,ss1,ss2
	character*11 xisra,xisdec,xistb,xisctb,xptime
	character*10 xpdate
	character*8 cxiang,cxeang,cphase,inm
	integer*4 xicflg(32)
	integer*2 bitnum,chkbit,pbit,tbit

	pbit=3				#Ra,dec flag
	tbit=5				#civil,universal flag 
#
# id set to zero to initialize it (don't know why nits here- RNC 2/25/93
#
	id =0
#
# convert angles to proper format
#
	intsph=2000000000

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
	
#
# Put appropriate variables into dd:mm:ss format
#

	call todms(isra,24000,ih,im,ss)
	call todms(isdec,24000,ihh,imm,sss)
	call todms(istb,24000,ih1,im1,ss1)
	call todms(isctb,24000,ih2,im2,ss2)
	write(xisctb,101)ih2,im2,ss2
	write(xisra,101)ih,im,ss
	write(xisdec,101)ihh,imm,sss
	write(xistb,101)ih1,im1,ss1
	call zeroin(xisctb)
	call zeroin(xisra)
	call zeroin(xisdec)
	call zeroin(xistb)
101	format(I2,':',I2,':',F5.2)

#
# Put icflag into array xicflg
#
	do i = 0,31{
		bitnum = i
		xicflg(i+1)= chkbit(icflag,bitnum)
	}
#
# Write out header info
#

	call plpage(lpline,inm)
	write(lstlun,10) irecno,ititl,itchan,ihist 

# RED - switched arguments in chkbit call
#	if (chkbit(tbit,icflag)==1) {
	if (chkbit(icflag,tbit)==1) {
		write(lstlun,15) xpdate,xisctb,istb,scatim,timint
	}else{
		write(lstlun,20) xpdate,xisctb,istb,scatim,timint
	}

# RED - switched arguments in chkbit call
#	if (chkbit(pbit,icflag)==1) {
	if (chkbit(icflag,pbit)==1) {
		write(lstlun,40) xisra,xisdec
	}else{
		write(lstlun,45) xisra,xisdec
	}
	lpline=lpline+3
	call plpage(lpline,inm)
	write(lstlun,50) cxiang,cxeang,cphase,temp
	write(lstlun,60) irwav,itpntr,irespt,revs
	write(lstlun,65) xnrm,itimch,irmas,nruns,iwtrns
	write(lstlun,70) iband(1),iband(2),(xicflg(i),i=1,32)
	lpline=lpline+4
	call plpage(lpline,inm)
	if(ic == 1)  {
	 	write(lstlun,300) 
		lpline=lpline+1
		for(i=1;i<224;i=i+73) {
			if (mhist(i:i+4)!='     ') {
				write(lstlun,25) mhist(i:i+73)
				lpline=lpline+1
			}
		}



	 	write(lstlun,300) 
		lpline=lpline+1
	}
	if (ic+id==0) write(lstlun,5)
	if (ic+id==0) lpline=lpline+1
	call plpage(lpline,inm)
	write (lstlun,5)
	lpline=lpline+1
	
#
#
5	format(1x)
10	format(1x,I6,2x,a,2x,'channels:',2x,I6,2x,a)
15      format(1x,4x,'Date: ',a,4x,'Universal time= ',a,4x,'Sid time ',I6,2x,
	'scan time= ',F7.2,2x,'int time= ',F7.2)
20	format(1x,4x,'Date: ',a,4x,'Civil time= ',a,2x,'Sid time= ',I6,2x,
	'scan time= ',F7.2,2x,'int time= ',F7.2)
25 	format(5x,a)
40	format(5x,'latitude= ',a,3x,'longitude= ',a)
45	format(1x,4x,'right ascension= ',a,2x,'declination= ',a)
50	format(1x,4x,'inc.angle= ',a,3x,'emiss. angle= ',a,3x,
		'phase angle= ',a,2x,'temperature= ',F7.3)
60	format(1x,4x,'wave pntr: ',I4,2x,'text pntr: ',I4,2x,
		'resol pntr: ',I4,2x,'revs= ',I4)
65      format(1x,4x,'xnrm= ',f7.3,2x,'chop(msec)= ',i4,2x,'airmass= ',f7.3,
	'nruns= ',I6,2x,'wruns= ',I6)
70	format(1x,4x,'band normalization( ',I6,', ',I6,' )',2x,'bit flags: ',2x,	32I1) 
100	format(f8.4)
120	format(5x,a)
105	format(f8.3)
300     format(6x,75('-'))

return
end
 
	

 
