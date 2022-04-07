	subroutine plot(imode,invxp,nwv,xmax,xmin,ymax,ymin,data,wav,line)
	implicit integer*4 (i-n)
#
	dimension data(256),wav(256)
	logical invxp

	iline = 2
	if (imode !=2) {
		call hpline(1)
		call alplty(ymin,ymax)
		call xplta(invxp,nwv,xmax,xmin)
	}
	diff = ymax-ymin
	call hpline(line)
	call ptplot(nwv,xmax,xmin,ymin,diff,data,wav,iline)
	return
	end
