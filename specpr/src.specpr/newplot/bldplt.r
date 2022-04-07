	subroutine bldplt
#ccc  name: bldplt
#ccc  version date: 
#ccc  author(s): Roger N. Clark and Jeff Hoover
#ccc  language: ratfor
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
	implicit integer*4 (i-n)

#ccc	Version: %W% %G% %U%

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/pltcnt"
	include "../common/plotspool"
	include "../common/label1"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/sitelogo"
	
	integer*4	thsplt,start,pltnum,overly,i,iflag,lstfil
	integer*4		system
	integer*4	ier, idummy, itmprc
	integer*4	iy,iday,imon, jd, isec, ihr,ihm,iss
	character*80	spool
	character*1	filnam(20)
	character*40	cmdfil
	character*20	temp20
	real*4		savmin,savmax

	data	filnam/' ',' ',' ','u','y',' ',' ','d','v','w',
				   ' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
#
#  assign lstfil to lstlun (normally used for printer listings, but not here)
#
	lstfil = lstlun

	write (ttyout, '(" Building plots")')
	thsplt = 1
	while (thsplt<=nplots) {
		call spinit
		overly = 0
		if (wminp==wmaxp) {
			start = thsplt
			wminp = 1.0e34
			wmaxp = -1.0e34
			while (!newplt && start<=nplots) {
				read (pltlun,rec=start,iostat=ier) dataa,datab,datac,opt
				if (ier!=0) return
				start = start+1
				if (wvmn<wminp) wminp = wvmn
				if (wvmx>wmaxp)	wmaxp = wvmx
			}
		}
		savmax = wmaxp
		savmin = wminp
		call setwav
		call setup(ttyout,pltfil,'re',27.3,26.0,iflag)
		call range(vmaxp,wminp,1.0,4.0,vminp,wmaxp,18.0*vscale,
						25.0*hscale)
		call axis(vmaxp,wminp,vminp,wminp,vlabel)
		call axis(vminp,wminp,vminp,wmaxp,xlabel)
		call axis(vminp,wmaxp,vmaxp,wmaxp,' ')
		call axis(vmaxp,wmaxp,vmaxp,wminp,' ')
#
# print logo and date at top of plot
#
	call symbol (0.0,10.0,-7,logo,90.)
#
# get date
#
	call jdatim (jd, isec)
	call frjuld (iy,imon,iday,jd)
	call todms(isec,1,ihr,ihm,ss)
	write (temp20,103) imon,iday,iy,ihr,ihm
	do itemp = 1, 10 {
		if (temp20(itemp:itemp) == ' ') temp20(itemp:itemp) = '0'
	}
	do itemp = 13, 17 {
		if (temp20(itemp:itemp) == ' ') temp20(itemp:itemp) = '0'
	}
	call symbol (0.4,12.5,-7,temp20,90.)

		pltnum = 0
		newplt = .false.
		down = 22.0
		while (!newplt && thsplt<=nplots) {
			pltnum = pltnum+1
			write (ttyout,100) thsplt,nplots
			read (pltlun,rec=thsplt,iostat=ier) dataa,datab,datac,opt
			if (ier!=0) return
			itmprc = recno
			call redfil(itmprc,filid,ier)
			if (ier!=0) {
				print *,"read error on spectrum file"
				thsplt = thsplt+1
				break
			}
			thsplt = thsplt+1
			write (ttyout,200) ititl,filnam(filid+1),recno
			do iitmp = 1, itchan {  #restore right data array
				data(iitmp) = datac(iitmp)
			}
			call doplot(pltnum)
		}
		if (more>9 || more<1) more = 1
		inquire(lstfil, name=cmdfil)
		write (lstfil,300)more
		close (lstfil,iostat=idummy)
		open (lstfil,file=NULL,iostat=idummy)
		close (lstfil,iostat=idummy)
		call done(1)
		wmaxp = savmax
		wminp = savmin
#
# close text and vector files assigned to logical units 1 and 2
#
		close (1, iostat=idummy)
		close (2, iostat=idummy)
#
# KEL 2/12/13
# following 'OLD STYLE'string write concatinations with slashes don't work with Linux,
#    replaced with modern string concatination without the write-statement formatting
		if (penplt==1) {
#			write(spool,"('/usr/local/bin/sp_spool -d hp HP9872 ',a)") cmdfil
			spool = "/usr/local/bin/sp_spool -d hp HP9872 " // cmdfil
		} else if (penplt==2) {
#			write(spool,"('/usr/local/bin/sp_spool -d hp HP7550 ',a)") cmdfil
			spool = "/usr/local/bin/sp_spool -d hp HP7550 " // cmdfil
		} else if (penplt==3) {
#			write(spool,"('/usr/local/bin/sp_spool -d hp Postscript ',a)") cmdfil
			spool = "/usr/local/bin/sp_spool -d hp Postscript " // cmdfil
		} else if (penplt==4) {
#			write(spool,"('/usr/local/bin/sp_spool -d hp hpgl ',a)") cmdfil
			spool = "/usr/local/bin/sp_spool -d hp hpgl " // cmdfil
		} else if (penplt==5) {
#			write(spool,"('/usr/local/bin/sp_spool -d hp tgif ',a)") cmdfil
			spool = "/usr/local/bin/sp_spool -d hp tgif " // cmdfil
		} else {
#			write(spool,"('/usr/local/bin/sp_spool -d vp0 -splot GOULD ',a)") cmdfil
			spool = "/usr/local/bin/sp_spool -d vp0 -splot GOULD " // cmdfil
		}
		i = system(spool // char(0))	
		call fsleep (1)	# sleep one second so fast machines
				# won't generate more than one plot
				# in one second.  That way the "unique"
				# plot file names will remain unique.
	}
	return
100	format ('spooling plot ',i3,' out of ',i3)
103	format (i2,'/',i2,'/',i4,'  ',i2,':',i2,' UT')
200	format ('plotting ',a,'  file ',a1,' record ',i6)
300	format ('C',i4)
	end
