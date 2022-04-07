	subroutine rstart0 (ic)
	implicit integer*4 (i-n)
	integer*4 ic
#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#
#     this subroutine sets up the specpr files and protections for
#     restarting the program at the same condition as at the time of
#     the rstart call or it also stores the parameters for a future
#     restart.
#
#      ic= 1: store current parameters for a future restart.
#      ic= 2: restart= recall the parameters and assign and open all
#                      the files and devices.
#      ic= 3: recall the parameters but don't assign and open any files
#ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	include "../common/spmaxes"   # max parameters, must be first

	common /blank/  dataa(512)
	common /lblvol/ lblvol(160)
	common /lblprt/ lblprt(6)
	common /label3/ label3(6)
	common /labelf/ labelf(11)
	common /lblg/   lblg(5)
	common /lbl4/	lbl4(58)  # note: until 5/3/1993 this was 73
	common /info/   info(12)
	common /cmd/    icmd(2)
	common /lbl7/	lbl7(25)

	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/ioftyp"
	include "../common/cmdarg"

	integer		i
	integer*4	dataa
	integer*4	ier, idummy
	integer*4	ioftp(127)
	character*80    files(8)
	equivalence 	(files,lblvol)
	equivalence	(filtyp(1,1),ioftp(1))

	if (ic==1) {

		write (ttyout,51)
#
#     store parameters for a future restart
#
		do  i=1,160
			dataa(i)=lblvol(i)

		do i=1,6
			dataa(160+i)=lblprt(i)

		do i=1,6
			dataa(166+i)=label3(i)

		do i=1,11
			dataa(177+i)=labelf(i)

		do i=1,5
			dataa(188+i)=lblg(i)

		do i=1,12
			dataa(193+i)=info(i)

		do i=1,2
			dataa(205+i)=icmd(i)

		dataa(208)=igrmod

		do i=1,58
			dataa(208+i)=lbl4(i)  # note: space for 73
                do i=59,73
			dataa(208+i)=0.0 # note: space holder for 73

		do i=1,3
			dataa(281+i)=lbl7(22+i)

		do i = 1, 127
			dataa(284+i)=ioftp(i)

		write(rlun,rec=1,iostat=ier) dataa
		if (ier!=0) goto 4321

		return

4321		write(ttyout,4322) ier
4322		format ( 'write error on restart file, error=', i5, /, 
			' Terminating specpr')
		stop
	} else {
#
#
#----------------------------------------------------------------------
#
#     do a restart:
#
#     allocate the parameter file, recall parameters and allocate
#     and open all files and devices as appropriate.
#

		write (ttyout,50)

		close(rlun,iostat=idummy)

		if (ncmdarg >= 1) {
			files(8) = charg1
			write (ttyout, 30) files(8)
30			format ('restart file= ', a)
		}
		open(rlun,file=files(8),iostat=ier,
			access='direct',recl=2048,form='unformatted')
		if (ier != 0) goto 1234

		read(rlun,rec=1,iostat=ier)dataa
		if (ier!=0) goto 1234
		do  i=1,160
			lblvol(i)=dataa(i)

		do i=1,6
			lblprt(i)=dataa(160+i)

		do i=1,6
			label3(i)=dataa(166+i)

		do i=1,11
			labelf(i)=dataa(177+i)

		do i=1,5
			lblg(i)=dataa(188+i)

		do i=1,12
			info(i)=dataa(193+i)

		do i=1,2
			icmd(i)=dataa(205+i)

		igrmod=dataa(208)
	
		do i=1,58
			lbl4(i)=dataa(208+i)  # note: space for 73

		do i=1,3
			lbl7(22+i)=dataa(281+i)


# don't initialize 61 to 79 because that is ftptr which is set by blockdata
		do i = 1, 60
			ioftp(i)=dataa(284+i)
		do i = 80, 127
			ioftp(i)=dataa(284+i)

#------------------------------
	}
	if (ic==3) return

	open(cmdlun,file=COMMAND,iostat=idummy,
		access='direct',recl=80,form='unformatted')
	if (idummy!=0) {
		write(ttyout,100)
		stop
	}
#
#     assign title file
#
	open(ttllun,file=TITLE,iostat=idummy,
		access='direct',recl=128,form='unformatted')
	if (idummy!=0) {
		write(ttyout,400)
		stop
	}
#
#     assign addition scratch file
#
	open(addlun,access='direct',recl=19456,form='unformatted',
		iostat=idummy,status='scratch')
	if (idummy!=0) {
		write(ttyout,500)
		stop
	}
#
#     assign plot scratch file
#
	open(pltlun,access='direct',recl=80,form='unformatted',
		iostat=idummy,status='scratch')
	if (idummy!=0) {
		write(ttyout,600)
		stop
	}
#
#     assign device v
#
	inn = 4
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(1)!=NULL) open(vlun,file=files(1),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(1)
		files(1) = NULL
	}
#
#     assign device w
#
	inn = 5
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(2)!=NULL) open(wlun,file=files(2),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(2)
		files(2) = NULL
	}
#
#     assign device d
#
	inn = 3
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(3)!=NULL) open(dlun,file=files(3),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(3)
		files(3) = NULL
	}
#
#     assign starpack file =s
#
	if (files(4)!=NULL) open(slun,file=files(4),iostat=idummy,
		access='direct',recl=77824,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(4)
		files(4) = NULL
	}
#
#     assign device u
#
	inn = 1
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(5)!=NULL) open(ulun,file=files(5),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(5)
		files(5) = NULL
	}
#
#     assign device y
#
	inn = 2
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (files(6)!=NULL) open(ylun,file=files(6),iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) files(6)
		files(6) = NULL
	}
#
#     assign listing device: lp or dummy
#
	if (files(7)!=NULL && files(7)!=SPLFILE) open(lstlun,file=files(7),
		iostat=idummy,form='formatted')
	if (idummy!=0) {
		write(ttyout,700) files(7)
		files(7) = NULL
	}
#----------------------------------------------------------------------
#
	return
#
1234    write(ttyout,"('read error on restart file ier=',i5)")ier
	stop
#**********************************************************************
50	format(' Restarting ')
51	format(' Updating Restart File ')
100	format('Can''t open command history file (.cmd). exiting!!')
400	format('Can''t open title storage file (.spttl). exiting!!')
500	format('Can''t open addition scratch file. exiting!!')
600	format('Can''t open plot scratch file. exiting!!')
700	format('Can''t open data file ',a,/,
			'reseting it to /dev/null')

#
	end
