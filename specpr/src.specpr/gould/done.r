	subroutine done(i)
	implicit integer*4 (i-n)

#ccc	Version: %W% %G% %U%

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/filenames"
	include "../common/plotspool"
	include "../common/pltcnt"

	character	tmpfil*29, cmdfil*30

	data	cmdfil	/PLTCMD/

	if (i!=0) {
		cmdfil(20:30) = pltfil(19:29)
		if (penplt != 0) {
			cmdfil(12:12) = 'h'
		}
	}
	return
	end
