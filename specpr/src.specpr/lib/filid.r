	subroutine filid (lic, ifilnu, ifilid)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc           This subroutine decodes the file id, number and
#ccc           option
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    decode1
#ccc  argument list description:
#ccc      arguments: lic, ifilnu,ifilid
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
########################################################################
#                                                                      #
#    this subroutine decodes the file id, number and options           #
#                                                                      #
#      arguments:                                                      #
#      lic:    (input) starting location in iopcon                     #
#      ifilnu: (output)record number if found. 0 otherwise.            #
#      ifilid: (output)file id if found. 0 otherwise.                  #
#                                                                      #
########################################################################
	include "../common/lbl4"

#RED
	integer*4 ihchar      # function ihchar

	character*1 itmp


	ifd= 0
	iops = ' '

	j= 1
	jtmp = lic
	do lic= jtmp, 80 {
		i=lic
		if(lic>80) break
		else if (iopcon(i:i)==' ') next
		else if (iopcon(i:i)=='p') {
			iops(j:j) = 'p'
			j= j+ 1
		}
#
#     decode file id
#
		ifilid= 0
		itmp = iopcon(i:i)
		if (itmp=='w' |
		    itmp=='v' |
		    itmp=='d' |
		    itmp=='s' |
		    itmp=='u' |
		    itmp=='y') {
			ifilid = ihchar(itmp)
			break
		}
	}
	if(i==80) {
		iops(1:1) = 'x'
		return
	}
	i= i+ 1
	ifilnu= 0
	jtmp = i
	do i= jtmp, 80 {
		if (iopcon(i:i)==' ') next
		else if ((iopcon(i:i)<'0').or.(iopcon(i:i)>'9')) break
		call decod1 (iopcon(i:i),ifd)
#
#     decode file number
#
		ifilnu= ifilnu*10+ ifd
	}
	jtmp = i
	for (i = jtmp; i <= 80; i = i+1) {
		if (iopcon(i:i)==' ') next
		iops(j:j) = ' '
#
#     decode options
#
		itmp = iopcon(i:i)
		if (itmp=='w' |
		    itmp=='v' |
		    itmp=='d' |
		    itmp=='s' |
		    itmp=='u' |
		    itmp=='y') break
		iops(j:j)=itmp
		j= j+ 1
		if (j==39) i= i+ 1
		if (j>=39) break
	}
	lic= i
	return
	end
