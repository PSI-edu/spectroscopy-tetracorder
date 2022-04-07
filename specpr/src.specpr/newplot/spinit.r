	subroutine spinit
	implicit integer*4 (i-n)

#ccc  version date: 10/18/83 %W% %G% %U%
#ccc  author(s): Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc		this routine initializes the plot spooling
#ccc		files.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc             tname
#ccc  argument list description:
#ccc       arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
	

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/lundefs"
	include "../common/filenames"
	include "../common/plotspool"
	include "../common/pltcnt"

	character	l*29, cmdfil*30
	character	temp*2
	integer*4	ftime
	integer*4       idummy

	data l		/PLTDAT/
	data cmdfil	/PLTCMD/

#
#  assign lstfil to lstlun (normally used for printer listings, but not here)
#
	lstfil = lstlun

	write(temp,'(i2)') pltcnt
	if (pltcnt<10) l(19:19) = temp(2:2)
	else l(19:20) = temp
	
	pltfil = l
	call tname(pltfil,23,ftime())
	
	cmdfil(20:30) = pltfil(19:29)

	open(lstfil,file=cmdfil,form='formatted', iostat=idummy)

	if (idummy!=0) {
		write(*,*) "Open error iostat=", idummy, "  file=", cmdfil
	}
	write(lstfil,100)pltfil(1:29),pltfil(1:29),
		cmdfil,pltfil(1:29),pltfil(1:29)

100	format('V',a,'v',/'T',a,'t',/,'R',a,/,'R',a,'v',/,'R',a,'t')
	
	return
	end
