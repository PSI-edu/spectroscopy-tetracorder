	subroutine rdtitl(lun)
    implicit integer*4 (i-n)

    include "../status"

	common/record/ ixdum
	character*1536 ixdum

	read(lun,rec=ifiler+1,iostat=ier) ixdum
    ititlr = ixdum

#    write(6,100)ititlr
#100 format(1x,'rdtitl... title = ',a)

	return
	end
