	subroutine setupreadsheet (qlun, filhdr, reclen, rechdr,
		idnoff, dx, dy, dz, numtyp, filorg, iptdrop, scale)
	implicit integer*4 (i-n)

#ccc  name: setupreadsheet
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: Initalize 3d file parameter variables:
#ccc		given logical unit, qlun, find all the
#ccc		headers, dimensions, etc of the 3d file.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: none
#ccc  argument list description:
#ccc     INPUT:
#ccc        qlun:   3D logical unit number
#ccc     OUTPUT:
#ccc        filhdr: file header length in bytes
#ccc        reclen: record length in bytes
#ccc        rechdr: record header length in bytes
#ccc        idnoff: DN offset
#ccc
#ccc        dx: x dimension in pixels = number of samples
#ccc        dy: y dimension in pixels = number of lines
#ccc        dz: z dimension in pixels = number of bands
#ccc
#ccc        numtyp: Data type 1= int*2
#ccc        filorg: file organization (1 = BIL)
#ccc        iptdrop: deleted point value
#ccc        scale: scale factor
#ccc		
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

        include         "../specpr/src.specpr/common/ioftyp"

	integer*4 qlun, filhdr, reclen, rechdr, idnoff
	integer*4 dx, dy, dz
	integer*4 numtyp, filorg, iptdrop
	real*4    scale

        filhdr = filtyp(2,ftptr(qlun))    # file header length in bytes
        reclen = filtyp(3,ftptr(qlun))    # record length in bytes
        rechdr = filtyp(4,ftptr(qlun))    # record header length in bytes
        idnoff = filtyp(5,ftptr(qlun))    # DN offset

#       Barry Middlebrook: Changed dx, dy, dz for Andrea 10/19/90
#       to match new specpr ordering DID NOT TEST    Barry J.

        dx = filtyp(7,ftptr(qlun)) # x dimension in pixels
        dy = filtyp(6,ftptr(qlun)) # y dimension in pixels
        dz = filtyp(8,ftptr(qlun)) # z dimension in pixels

        numtyp = filtyp(9,ftptr(qlun))    # Data type 1= int*2
        filorg = filtyp(10,ftptr(qlun))   # file organization (1 = BIL)
        iptdrop = filtyp(11,ftptr(qlun))  # deleted point value
        scale = dnscal(ftptr(qlun))       # scale factor

	return
	end
