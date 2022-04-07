#HPUX	program sp_stamp (ach1, ach2, ach3)
#IA64HPUX	program sp_stamp 
#
#************************* sp_stamp   *******************************
# 
# Randall Dailey 06/02/2004 
# This program put a specpr stamp on the specified file.
# Most of the source came from asciitosp.r and wrtspr.r
#
#********************************************************************
#
	implicit integer*4 (i-n)

# ../../src.specpr/common/...


	include  "../src.specpr/common/lbl4"
	include  "../src.specpr/common/label1"
	include  "../src.specpr/common/cmdarg"

	include  "../src.specpr/common/lblvol"
	include  "../src.specpr/common/ioftyp"
	include  "../src.specpr/common/pipes"
	include  "../src.specpr/common/delete"


#
	character*80 filnam
	character*1536 tdata
	equivalence (tdata,iobuff)
	integer*4 ier,key

#HPUX	character*80 ach1, ach2, ach3

	integer*4 fsize
	integer*4 recnum
	integer*4 idummy, filsiz
	integer*4 N,arglen
#
	luncrt = 6
	lun =10

#HPUX	charg1 = ach1
#HPUX	charg2 = ach2
#HPUX	charg3 = ach3

#
# Open crt for screen output
#
	open(16, file='/dev/null', access='direct',recl=80,
			iostat=ier,form='unformatted')
#
# Get number of arguments from command line after program.
#
	call getcmdargs

	if (ncmdarg.ge.1) filnam = charg1
	if (ncmdarg.ge.1) go to 10
#
	write (luncrt,5)
5      format (1x, 'Input file not fully specified after program name',
		/, 1x, 'Proper use is:  sp_stamp specpr_filname',/)
#
	go to 5000

#
#
10     open (lun,file=filnam,iostat=idummy,status='old',
		access='direct',recl=1536,
		form='unformatted')


       if (idummy.ne.0) {
		write (luncrt,15)idummy
15		format (1x, 'cant open file, error',1x, i6,/)
		stop
       }
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
# Initialize iobuff to all zeros
#
50	do i = 1,384 {
		iobuff(i) = 0
	}
 

#
# Set Stamp strings
#
        tdata(1:13) = 'SPECPR_FS=2.0'
	tdata(14:14) = char(13)
	tdata(15:15) = char(10)
	tdata(16:32) = 'RECORD_BYTES=1536'
	tdata(33:33) = char(13)
	tdata(34:34) = char(10)
        tdata(35:49) = 'LABEL_RECORDS=1'
	tdata(50:50) = char(13)
	tdata(51:51) = char(10)

#
# Write out first 1536 bytes again
#
	key = 1 
	ier = 0
	write(lun,rec=key,iostat=ier)iobuff
	if (ier!=0) {
	write(luncrt, 100) ier
100              format (' ERROR: File Request',i5,/)
	}


#
# Done
#
5000    close (10)
	stop
#
	end
#
