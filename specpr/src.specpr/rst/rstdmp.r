	subroutine	rstdmp
	implicit integer*4 (i-n)
	
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/info"
	include "../common/lblvol"
	include "../common/lblprt"
	include "../common/hptrm"

	character*40 date
#
#********* restart file dump ******************************
#
	data ihv,ihw,ihd,ihs,ihu,ihy/1hv,1hw,1hd,1hs,1hu,1hy/
	data ihl,ihp,ihr/1hl,1hp,1hr/
#	data ispa/' '/
	ispa = ihchar(' ')
#
	call rstart(2)
	call setspo
	write(12,100)
100	format(82('*'),/,34('*'),' RESTART FILE ',35('*'),/,83('*'),///)
	call ixdate(date)

	write(6,1)
1	format(///,'>>>>>>> dumping restart file <<<<<<<<<<')
	write(12,105)irfl
105	format(/,2x,'Restart File Name: ',a,//)
	write(12,110)
110	format(5x,'File:',t14,'Assigned to:',t62,'Tape Name:',t76,'Protection:')

	write(12,121)ihv,ivfl,isavt,iprtv
121	format(7x,a,7x,a,7x,a,7x,i5)
	write(12,121)ihw,iwfl,iwdgt,iprtw
	write(12,121)ihd,idfl,iwrkt,iprtd
	write(12,121)ihu,iufl,inmu,iprtu
	write(12,121)ihy,iyfl,inmy,iprty

	write(12,122)ihs,isfl,iprts
122	format(7x,a,7x,a,7x,8x,7x,i5)
	write(12,123)ihl,ilfl
123	format(7x,a,7x,a)

	write(12,125)
125	format(//,1x,'Current Status of Files:')
	write(12,126)isavf,iwjf,iwrkf,istrf,isvcu,iwjcy
126	format(3x,'v = f',i5,
		8x,'w = f',i5,8x,'d = f',i5,8x,'s = f',i5,8x,'u = f',i5,
		8x,'y = f',i5)
	write(12,128)nchans,ibnrm1,ibnrm2,nchsav
128	format(/,1x,'Number of Channels:',i5,/,
		1x,'Lower and Upper Channel Limits for Band Norm.:',i5,',',i5,
		/,1x,'Number of Channels for Wavelength file 1:',i5/)
	write(6,12)
12	format('<<<<<<<< finished >>>>>>>>>')
	call dumpsp
	end
