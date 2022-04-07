	subroutine xbin (inpchn, datain, dataw, outchn, dataou, iflag)
	implicit integer*4 (i-n)

#ccc  name: xbin
#ccc  version date: 8/12/86
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: bin input data (datain) to bins defined by dataw
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

	include "../common/lundefs"

	integer*4 inpchn, bgnchn, outchn, iflag, i
	real*4 datain(inpchn), dataw(outchn), dataou(outchn)
	real*4 tmp

# check that dataw is sequentially increasing.

	tmp = -1.23e34
	ibmax=0
	do i = 1, outchn {
		if ((dataw(i) != -1.23e34) && (dataw(i) <= tmp)){
			write (ttyout, 100) i
			iflag = -1          # FATAL: exit
			call crtin
			return
		}
		tmp = dataw(i)
	}

# do find first channel

# RED Intialize to 0
	bgnchn = 0
	do i = 1, outchn {
		if (dataw(i) != -1.23e34) {
			bgnchn = i
			break
		}
	}
	if (bgnchn >= outchn) {
		write (ttyout, 110)
		iflag = -1
		call crtin
		return
	}

        bmin = -1.0e34  # bin minimum boundary
	ibnext = 0

        do  i = bgnchn, outchn {
		if (dataw(i) != -1.34e34) {
			ibnext = i
			break
		}
	}
	if (ibmax != 0) {
		write (ttyout, 110)
		iflag = -1
		call crtin
		return
	}

	bmax = (dataw(bgnchn) + dataw(ibnext))/2.0
	dataou(bgnchn) = 0.0

	do i = 1, inpchn {    # accumulate first bin
		if ((datain(i) < bmax) && (datain(i) != -1.23e34)) {
			dataou(bgnchn) = dataou(bgnchn) + 1.0
		}
	}

# now do other bins

	iblast = ibnext
	jstart = ibnext
	bmin = bmax
	do j = jstart, outchn {

		if (dataw(j) == -1.34e34) next
		ibnext = j
		if (ibnext == outchn) break

		bmax = (dataw(iblast) + dataw(ibnext))/2.0
		dataou(bgnchn) = 0.0

		do i = 1, inpchn {    # accumulate bin
			if ((datain(i) >= bmin) && (datain(i) < bmax) && 
						(datain(i) != -1.23e34)) {
				dataou(j) = dataou(j) + 1.0
			}
		}
		iblast = ibnext
		bmin = bmax
	}
	if (dataw(ibnext) != -1.23e34) {

		do i = 1, inpchn {    # accumulate bin
			if ((datain(i) >= bmin) && (datain(i) != -1.23e34)) {
				dataou(bgnchn) = dataou(bgnchn) + 1.0
			}
		}
	}
	return

	

#################
# 
100	format (' FATAL ERROR: output wavelength set not ordered at ch',
                i6,/,' press return to exit',/)

110	format (' FATAL ERROR: insufficient non-deleted output channels',
		/,' press return to exit',/)

	end
