#  6.11.80  dmc  gfplot  sfortran
#
#  this routine is the sub-monitor for the 1980 version of gfit plotting
#  package. its responsibilities are :
#
#      1: collects commands from terminal and calls appropriate
#         subroutines, or prints an error message.
#  commands are listed around line 304.
#
#  that's all.
#-----------------------------------------------------------------------
#
	subroutine gfplot
	implicit integer*4 (i-n)
	include "../status"
#RED
	real*4 bas1    # function

	common/label1/ xdum(384)
	include "../../common/lbl4"

	dimension data(256), errors(256), wavel(256)
	dimension d(256)

	character*2 cmd,mcom
	character*2 ibl
	character*2 icom(14)
	equivalence (d(1),  errors(1))

	logical range, lsave, invchg, logchg
	logical good,hfo, chgrw

	data ibl/'  '/
	data delete/-1.23e+34/, maxcom/12/

	data icom/"rp","g-","ga","co","o-",
	          "i-","ie","oe","e","li","c","il","  ","  "/
	data iht/1ht/, iy/1hy/, in/1hn/
	data me/6/

	logchg = .false.
	invchg = .false.
	chgrw = .true.
	ip = 1
#-----------------------------------------------
#  ipack = 1 means that the spectra and wavelength
#  values returned will be compressed. this means that
#  deleted points are dropped from the returned arrays
#-----------------------------------------------------------

	ipack = 1

	call er
	repeat {

	if (logchg .or. invchg .or. chgrw) {
	    call rdspec(wavel,data,errors,nwv,ipack)

	    if (invxp) {
			do i = 1,nwv {
					if (wavel(i) == 0.0) write(6,999) i
					if (wavel(i)!=0.0 && wavel(i)!=delete) 
						wavel(i) = 1. / wavel(i)
			}
	    }

	    if (logp) {
			do i = 1,nwv {
				if (data(i) .ne. delete) {
					if (data(i) <= 0.0) write(6,1234) data(i)
					if (data(i) > 0.0) data(i) = alog(data(i))
				}
			}
	    }


	}

	if ((xmin .eq. xmax)  .or. (invchg) ) {
	    invchg = .false.
	    xmax = -1.e+30
	    xmin = -xmax

	    do i = 1,nwv {
			if (data(i) .ne. delete) {
				xmin = amin1(xmin,wavel(i))
				xmax = amax1(xmax,wavel(i))
			}
	    }
	}

	if ((ymin .eq. ymax) .or. (logchg)) {
	    logchg = .false.
	    ymin = 1.e+30
	    ymax = -ymin
	    do i = 1,nwv {
			if (data(i) .ne. delete) {
				ymin = amin1(ymin,data(i))
				ymax = amax1(ymax,data(i))
			}
	    }
	}
	if (chgrw) {
	    chgrw = .false.
	    call movabs(0,300)
	    call sb(0)
	    write(6,30) label,idr,ifiler,ititlr
	    line = 1
	    call plot(1,invxp,nwv,xmax,xmin,ymax,ymin,data,wavel,line)
	    call movabs(350,340)
	    call drwabs(390,340)
	    call movabs(395,340)
	    call sb(0)
	    write(6,31)
	    line = 8
	    call pgaus(21,nwv,wavel,d)
	    call plot(2,invxp,nwv,xmax,xmin,ymax,ymin,d,wavel,line)
	    call movabs(350,315)
	    call drwabs(390,315)
	    call movabs(395,315)
	    call sb(0)
	    write(6,32)
	}
	call movabs(0,350)
	call sb(0)
	write(6,199)
	call movabs(0,350)
	call sb(0)

	repeat {
	    call crtin
		cmd = iopcon(1:2)
		mcom = cmd
	    if (mcom .eq. ibl) mcom = icom(14)

	    good = .false.
	    do k = 1,maxcom {
		    if ( mcom .eq. icom(k)) {
		        ivarnm = k
		        good = .true.
		    }
	    }
	    if (.not. good) {
		    write(me,250)
	    }

	} until (good)

	if (ivarnm == 1) {
		call er
		chgrw = .true.
	} else if (ivarnm == 2 || ivarnm == 3) {
		range = .false.
		nchar=3
		call wjfren(nchar,x,il)
		ig1 = x
		if (nchar >= 80) goto 177
		call wjfren(nchar,x,il)
		if (il == iht) range = .true.
		if (range) {
		    call wjfren(nchar,x,il)
		    ig2=x
		    for(i=ig1;i<=ig2;i=i+1) {
				im=i
				if (ivarnm== 3) im = -i
				call movabs(0,300)
				call sb(0)
				call pgaus(im,nwv,wavel,d)
				line = mod(i,7) + 4
				call plot(2,invxp,nwv,xmax,xmin,ymax,ymin,d,wavel,line)
		    }
		} else {
		    if (ivarnm== 3) ig1 = -ig1
		    call movabs(0,300)
		    call sb(0)
		    call pgaus(ig1,nwv,wavel,d)
		    line = mod(ip,7) +4
		    ip = ip+1
		    call plot(2,invxp,nwv,xmax,xmin,ymax,ymin,d,wavel,line)
		}
177		continue
	} else if (ivarnm == 4) {
		call movabs(0,300)
		call sb(0)
		write(me,300)
		call pgaus(0,nwv,wavel,d)
		line = 1
		call plot(2,invxp,nwv,xmax,xmin,ymax,ymin,d,wavel,line)
	} else if (ivarnm == 5) {
#-----------------------------------------------------------
#  plot fit - continuum
#----------------------------------------------------------
		call movabs(0,300)
		call sb(0)
		write(6,301)
		call shuffl(igaus,params)
		i = 1
		while (i .le. nwv) {
		    nwh = 3 * igaus + icont + 1
		    d(i) = f(nwh,wavel(i),params,icont,igaus)
		    d(i) = d(i) - bas1(wavel(i),icont,igaus,params)
		    i = i + 1
		}
		call shuffl(igaus,params)
		line = 5
		call plot(2,invxp,nwv,xmax,xmin,ymax,ymin,d,wavel,line)
	} else if (ivarnm == 6) {
#------------------------------------------------------
#  plot data - continuum
#----------------------------------------------------------
		call movabs(0,300)
		call sb(0)
		write(6,302)
		call shuffl(igaus,params)
		i = 1
		while (i .le. nwv) {
		    if (data(i) .ne. delete) {
				d(i) = data(i) - bas1(wavel(i),icont,igaus,params)
		    } else {
				d(i) = data(i)
		    }
		    i = i + 1
		}
		call shuffl(igaus,params)
		line = 6
		call plot(2,invxp,nwv,xmax,xmin,ymax,ymin,d,wavel,line)
#
#-----------------------------------------------------------
#  now recover the errors if they got clobbered
#------------------------------------------------------------
		if (ierror) {
		    if1 = ifiler + 1
		    call readsp(lunin,xdum,if1,iflg)
		    nch = nwv
		    call ptdel(xdum,errors,wavel,nch,ipack)
		}
	} else if (ivarnm == 7) {
		call movabs(0,300)
		call sb(0)
		write(6,123)
	} else if (ivarnm == 8) {
		call movabs(0,300)
		call sb(0)
		write(6,124)
	} else if (ivarnm == 9) {
		call hreset(1)
		call hpline(1)
		return
	} else if (ivarnm == 10) {
		call er
		write(6,160)  (icom(k),k=1,maxcom)
		call crtin
		chgrw = .true.
		call er
	} else if (ivarnm == 11) {
		call scale(ymin,ymax,nwv,data,errors,xmin,xmax,i)
		chgrw = .true.
	} else if (ivarnm == 12) {
		call er
		if (invxp) write(6,2000) iy
		else write(6,2000) in
		call crtin
		nchar = 1
		call wjfren(nchar,x,il)
		lsave = invxp
#RED Changed lsave == 0 to lsave .eqv. .false.
		if (il == iy & lsave .eqv. .false.) {
		    invxp = .true.
		    invchg = .true.
		} else if (il == in & lsave) {
		    invxp = .false.
		    invchg = .true.
		}
		if (logp) write(6,2001) iy
		else write(6,2001) in
		call crtin
		nchar = 1
		call wjfren(nchar,x,il)
		lsave = logp
#RED Changed lsave == 0 to lsave .eqv. .false.
		if (il == iy & lsave .eqv. .false.) {
		    logp = .true.
		    logchg = .true.
		} else if (il == in & lsave) {
		    logp = .false.
		    logchg = .true.
		}
		chgrw = .true.
		call er
	}

	} until (hfo)
	return
30	format(1x,a,/,1x,a,i3,5x,a)
31	format(1x,'Input')
32	format(1x,'Output')
123	format(1x,'Input Errors (not implemented)')
124	format(1x,'Continuum Errors (not avail.)')
160	format(1x,' the following plotting commands are available:'/,
	 1x,a2,' -- replot graphics'/,
	 1x,a2,' -- plot gaussians - continuum'/,
	 1x,a2,' -- plot gaussians + continuum'/,
	 1x,a2,' -- plot continuum'/,
	 1x,a2,' -- plot output fit - continuum'/,
	 1x,a2,' -- plot input - continuum'/,
	 1x,a2,' -- plot input errors'/,
	 1x,a2,' -- plot input - continuum errors'/,
	 1x,a2,' -- exit from plotting package'/,
	 1x,a2,' -- list commands'/,
	 1x,a2,' -- scale axis',/,
	 1x,a2,' -- modify invxp and logp',//,
	 1x,'PRESS RETURN TO CONTINUE',/)
199	format(20(' '))
250	format(' no!'/)
300	format(1x,'Continuum',70(' '))
301	format(1x,'fit - continuum',55(' '))
302	format(1x,'data - continuum',55(' '))
999	format(///,'WARNING!!!   Wavelength ',i3,' is zero',/)
1234	format(1x,'WARNING!!!   Invalid data for log space...',f15.6)
2000	format(1x,'invert wavelength values (',a1,')?',/)
2001	format(1x,'plot in log space (',a1,')?',/)
	end
