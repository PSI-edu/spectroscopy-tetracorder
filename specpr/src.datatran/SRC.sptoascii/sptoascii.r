#     Ratfor
#********************************************************************
#                     sptoascii

#  This program converts data in specpr format to a readable
#  ascii format that can be easily changed or modified.
#
#********************************************************************
#
#HPUX	program sptoascii (ach1, ach2, ach3)
#IA64HPUX	program sptoascii
	implicit integer*4 (i-n)

	character*10 xdatea,xdateb
	character*13 xscta,xsctb,xstb

	include "../../src.specpr/common/spmaxes"
	include  "../../src.specpr/common/lbl4"
	include  "../../src.specpr/common/label1"
	include  "../../src.specpr/common/cmdarg"
#
	character*1 ichil
	character*40 itlsav
	character*80 filnam
	character*1536 dummy
	equivalence (dummy,ititl)
	integer*4 recnum,ier
	integer*4 fsize,arglen
	integer*4 idummy, filsiz
	integer*4 lunsp
	integer*2 ibit,chkbit,bitnum
	integer*4 itest(32)

#HPUX	character*80 ach1, ach2, ach3
#
#
	luncrt = 6
	lunin = 5

#HPUX	charg1 = ach1
#HPUX	charg2 = ach2
#HPUX	charg3 = ach3
#
	maxchn = 4852
	maxrec = 99999
	maxtxt = 19680
	lunsp = 10
#
#      get number of arguments from command line after program.
#
	call getcmdargs

	if (ncmdarg.ge.1) filnam = charg1
	if (ncmdarg.ge.1) go to 10
#
	write (luncrt,1)
1      format (1x, 'Input file not fully specified after program name',
		/, 1x, 'Proper use is sptoascii specpr_filname',
                        ' < sourcefilename',/)
#
	go to 5000
#
10     open (lunsp,file=filnam,iostat=idummy,status='old',
		access='direct',recl=1536,
		form='unformatted')
       if (idummy.ne.0) {
		write (luncrt,15)idummy
15		format (1x, 'cant open file, error',1x, i6,/)
		stop
       }
# open file for crtin (found in redsp subroutine)
#
	open(16,file='/dev/null',access='direct',recl=80,
		iostat=ier,form='unformatted')
#
# get current length of file, test if consistent with specpr file
#     and set initial output record number to the end of the data
#     already in the file.
#
	filsiz = fsize(filnam)
#
	if (mod(filsiz,1536).ne.0) {
		write (luncrt,30) filnam, filsiz
30		format (1x, a, " does not appear to be a specpr file",
			/, "length of", i9, 
			" is not a multiple of 1536",/)
		stop
	}
#
# Find the record length of the specpr data file
#
	ifsiz = filsiz/1536 - 1
	recnum = 0
#
# Increment and read records until end of specpr file 
#
50	recnum = recnum +1
	if(recnum > ifsiz) go to 5000

#
# read header info from standard input and put into specpr labeled
#      common
#
  	call redspr (recnum,lunsp,ier)
	if (ier != 0 )go to 5000

#
#  find value of bit flags and put into array itest(32)
#
	do i = 0, 31{
		bitnum = i
		itest(i+1) = chkbit(icflag,bitnum)
	}

	ibit = 1
	if(chkbit(icflag,ibit)==0){

		write(luncrt,100)ititl
100     	format('ititl: ',a) 

		write(luncrt,110)(itest(i), i=1,32)
110  		format('bit flags:',1x,32I1)

		write(luncrt,120)usernm
120     	format('usernm: ',a) 

		call todms(iscta,24000,id,im,sec)
		write(xscta,190)id,im,sec
		call zeroin(xscta)
190		format(I2,':',I2,':',F5.2)
		write(luncrt,210)xscta
210 		format('iscta: ',a)

		call todms(isctb,24000,id,im,sec)
		write(xsctb,190)id,im,sec
		call zeroin(xsctb)
		write(luncrt,220)xsctb
220		format('isctb: ',a)

		call frjuld(iy,imon,iday,jdatea)
		write(xdatea,195)imon,iday,iy
		call zeroin(xdatea)
195		format(I2,'/',I2,'/',I4)
		write(luncrt,230)xdatea
230		format('jdatea: ',a)

		call frjuld(iy,imon,iday,jdateb)
		write(xdateb,195)imon,iday,iy
		call zeroin(xdateb)
		write(luncrt,240)xdateb
240		format('jdateb: ',a)


		call todms(istb,24000,id,im,sec)
		write(xstb,190)id,im,sec
		call zeroin(xstb)
		write(luncrt,280)xstb
280		format('istb: ',a)

		write(luncrt,290)isra
290		format('isra: ',I12) 

		write(luncrt,300)isdec
300		format('isdec: ',I12) 
 
		write(luncrt,310)itchan 
310		format('itchan: ',I12) 
 
		write(luncrt,320)irmas 
320		format('irmas: ',I12) 

		write(luncrt,330)revs
330		format('revs: ',I12)

		write(luncrt,340)iband(1)
340		format('iband1: ',I12)

		write(luncrt,345)iband(2)
345		format('iband2: ',I12)

		write(luncrt,350)irwav
350		format('irwav: ',I12)

		write(luncrt,360)irespt
360		format('irespt: ',I12)

		write(luncrt,370)irecno
370		format('irecno: ',I12)

		write(luncrt,380)itpntr
380		format('itpntr: ',I12)

		write(luncrt,390)ihist
390		format('ihist: ',a)

		write(luncrt,400)mhist(1:74)
400		format('m1: ',a)  

		write(luncrt,402)mhist(75:148)
402		format('m2: ',a)  

		write(luncrt,404)mhist(149:222)
404		format('m3: ',a)  

		write(luncrt,406)mhist(223:296)
406		format('m4: ',a)  

		write(luncrt,480)nruns
480		format('nruns: ',I12)    

		write(luncrt,490)siangl
490		format('siangl: ',I12)    

		write(luncrt,500)seangl
500		format('seangl: ',I12)    

		write(luncrt,510)sphase
510		format('sphase: ',I12)    

		write(luncrt,520)iwtrns
520		format('iwtrns: ',I12)

		write(luncrt,530)itimch
530		format('itimch: ',I12)

		write(luncrt,540)xnrm
540		format('xnrm: ',F12.4) 

		write(luncrt,550)scatim
550		format('scatim: ',F12.4)

		write(luncrt,560)timint
560		format('timint: ',F12.4)

		write(luncrt,570)tempd
570		format('tempd: ',F12.4)


		
#
#--- now write data --------------------------
#
	
		do ichan = 1,itchan {
			if (data(ichan) == -1.23e+34) next
			write(luncrt,1000)ichan,data(ichan)
1000   			format(I4,1x,1PE13.6)
		}   
		write(luncrt,1011)
1011		format('stat:complete')
#
# End of if statement. Else takes care of cases in which the
# text flag is set.
#
	}else{
		write(luncrt,100)ititl
		write(luncrt,110)itest
		write(luncrt,4000)itext(1:itxtch)
4000		format('text:',/,a,'ZZZZZZZZZZZZZZZZZZZZ')
		write(luncrt,1011)
	}

# completed now go back to beginning and wait for the next spectrum
#
	go to 50
#
# done
#
5000    close (10)
#
	end
