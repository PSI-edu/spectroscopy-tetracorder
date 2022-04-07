	subroutine f4 (ic,ipcn,constx,ispfcn,iopsav)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine executes sequential processing
#ccc                   setup by f3. no user access.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    none
#ccc  argument list description:
#ccc     arguments: ic,ipcn,constx,ispfcn,iopsav
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

	include "../common/lbl4"
	include "../common/alphabet"


	character*80 ipcn
	character*10 iopsav


	if (ipc(1)==0) go to 1000
	ix= ipc(3)
	if (ix!=ihv && ix!=ihw && ix!=ihd &&
	    ix!=ihu && ix!=ihy) go to 1000
	ix= ipc(7)
	if (ipc(1)==ihf) go to 500
	if (ix!=ihv && ix!=ihw && ix!=ihd && ix!=ihc &&
	    ix!=ihs && ix!=ihu && ix!=ihy) go to 1000
	ipc(4)= ipc(4)+ ipc(6)
	ipc(8)= ipc(8)+ ipc(10)
	if (ipc(4)>ipc(5)) go to 1000
	if (ipc(8)>ipc(9) && ipc(7)!=ihc) go to 1000
	write(iopcon,10) ipc(3),ipc(4),ipc(1),ipc(7),ipc(8),iopsav
	if (ipc(7)==ihc) write (iopcon,11) ipc(3),ipc(4),ipc(1),constx,iopsav

14      ipcn= iopcon
	ic=4
	ispfcn=4
	return

500     ipc(4)= ipc(4)+ ipc(6)
	if (ipc(4)>ipc(5)) go to 1000
	write (iopcon,501) ipc(3),ipc(4),ipc(1),ipc(2),iopsav
	go to 14
1000    ic=ihe
	ipc(1)=0
	return
10      format (a2,i4,a2,a2,i4,a10,' ,f4')
11      format (a2,i4,a2,'c',f14.7,a10,' ,f4')
501     format (a2,i4,a2,i4,a10,' ,f4')
	end
