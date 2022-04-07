	subroutine filidw (lic, ifilnu, ifilid)
	implicit integer*4 (i-n)

#ccc  version date: 12/22/2009
#ccc  author(s): Roger Clark 
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc           This subroutine decodes the file id, number and
#ccc           option for wavelengths (capitol letter file ids)
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

	integer*4 lic, ifilnu, ifilid

	integer*4 i, ifd, j, jtmp

	ifd= 0
	iops = ' '

	#write (*,*) "DEBUG: in filidw, lic=", lic

	j= 1
	jtmp = lic
	do lic= jtmp, 80 {
		i=lic
		#write (*,*) "DEBUG: filidw: iopcon(",i,")=", iopcon(i:i)
		if(lic>80) break
		else if (iopcon(i:i)==' ') next
#
#     decode file id
#
		ifilid= 0
		itmp = iopcon(i:i)
		#write (*,*) "DEBUG: filidw: itmp=", itmp
		if (itmp=='W' |
		    itmp=='V' |
		    itmp=='D' |
		    itmp=='S' |
		    itmp=='U' |
		    itmp=='Y') {
			ifilid = ihchar(itmp)
			#write (*,*) "DEBUG: filidw: wave file id=", itmp
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

	#write (*,*) "DEBUG: filidw pos2"
#
# future: we don't need options here.  we could delete this
# 
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
	#write (*,*) "DEBUG: filidw at the end"
	return
	end
