	subroutine defowpt (j, il, igo)

	implicit none

#ccc  version date: 11/26/2018
#ccc  author(s): roger clark
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc            this subroutine defines 
#ccc            overlays, wavemength markers,
#ccc            3point band depths,
#ccc            tetracorder features
#ccc               in the plot routines
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc  argument list description:
#ccc     argumrnts: igo
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

include "../common/spmaxes"   # max parameters, must be first

include "../common/blank"
include "../common/lbl7"
include "../common/lbl6"
include "../common/lbl4"
include "../common/label1"
include "../common/labl2"
include "../common/lbl3"
include "../common/label3"
include "../common/labelf"
include "../common/lblg"
include "../common/hptrm"
include "../common/lundefs"
include "../common/alphabet"
include "../common/dscrch"
include "../common/overlys"
include "../common/sp3pfeat"
include "../common/tetfeat"
include "../common/wavemarks"
include "../common/deletep"
include "../common/spcolors"

character*80 outline        # for X-window writes
character*180 outlinb        # intermediate buffer for X-window writes

integer*4 iov        #overlay number
integer*4 iwm        #wavelength marker number
integer*4 il, i, j, itmpo, iero
integer *4 ipt, ibd, itw, igo
real*4 lc, rc, bb, lcmin, lcmax, bbmin, bbmax, rcmin, rcmax

	igo =0

# ZZZZZZ define
                                # define overlay
				if (j < 73 && il == iho && iopcon(j:j)=='v') {
					iero = 0
					if(iopcon(j-1:j+2) == "ov1=" ) {
						#write (*,*) "DEBUG: overlay point 2 ov1="
						j=j+3
						itmpo=1
						call getoverly(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "ov2=" ) {
						#write (*,*) "DEBUG: overlay point 2 ov2="
						j=j+3
						itmpo=2
						call getoverly(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "ov3=" ) {
						j=j+3
						itmpo=3
						call getoverly(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "ov4=" ) {
						j=j+3
						itmpo=4
						call getoverly(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "ov5=" ) {
						j=j+3
						itmpo=5
						call getoverly(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "ov6=" ) {
						j=j+3
						itmpo=6
						call getoverly(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if (iero > 0) {
						call what (j)
						go to 9999
					}

				}

#                               define wavelength marker, form= wm1= 1.234 2.31 4.56 ...
#                               added 9/2013

				#write (*,*) "DEBUG: wavelength mark point 1"
				if (j < 73 && il == ihw && iopcon(j:j)=='m') {
					iero = 0
					if(iopcon(j-1:j+2) == "wm1=" ) {
						#write (*,*) "DEBUG: wavelength mark point 2 wm1="
						j=j+3
						itmpo=1
						call getwavmark(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "wm2=" ) {
						#write (*,*) "DEBUG: wavelength mark point 2 wm2="
						j=j+3
						itmpo=2
						call getwavmark(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "wm3=" ) {
						j=j+3
						itmpo=3
						call getwavmark(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "wm4=" ) {
						j=j+3
						itmpo=4
						call getwavmark(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "wm5=" ) {
						j=j+3
						itmpo=5
						call getwavmark(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "wm6=" ) {
						j=j+3
						itmpo=6
						call getwavmark(j,itmpo,iero)
						go to 9999   # go to beginning
					}
					if (iero > 0) {
						call what (j)
						go to 9999
					}

				}

#       check if 3-point band depth calculator defined:
#             label      left1   left2  center1  center2   right1  right2
#       bd1= 1.04um C|W  0.958   0.986   1.02     1.05      1.080   1.110 box
#       added 11/23/2018 - R. Clark

				#write (*,*) "DEBUG: checking for 3-point band depth point 1"
				if (j < 73 && il == ihb && iopcon(j:j)=='d') {
					iero = 0
					#write (*,*) "DEBUG: band depth bd found"
					if(iopcon(j-1:j+2) == "bd1=" ) {
						#write (*,*) "DEBUG: band depth bd1="
						j=j+3
						itmpo=1
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd2=" ) {
						#write (*,*) "DEBUG: band depth bd2="
						j=j+3
						itmpo=2
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd3=" ) {
						#write (*,*) "DEBUG: band depth bd3="
						j=j+3
						itmpo=3
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd4=" ) {
						#write (*,*) "DEBUG: band depth bd4="
						j=j+3
						itmpo=4
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd5=" ) {
						#write (*,*) "DEBUG: band depth bd5="
						j=j+3
						itmpo=5
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd6=" ) {
						#write (*,*) "DEBUG: band depth bd6="
						j=j+3
						itmpo=6
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd7=" ) {
						#write (*,*) "DEBUG: band depth bd7="
						j=j+3
						itmpo=7
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd8=" ) {
						#write (*,*) "DEBUG: band depth bd8="
						j=j+3
						itmpo=8
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "bd9=" ) {
						#write (*,*) "DEBUG: band depth bd9="
						j=j+3
						itmpo=9
						call get3ptbd(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}

					go to 9999   # go to beginning as a bd= can be the only thing on the line
				}

# tetracorder feature definitions

# tf1= Snow.H2O f1a DLw 0.958   0.986   1.080   1.110  ct 0.08
# tf2= Snow.H2O f2a DLw 1.150   1.178   1.315   1.345  ct 0.08 lct/rct> 0.9 1.1

				#write (*,*) "DEBUG: checking for tetracorder feature definition"
				if (j < 73 && il == iht && iopcon(j:j)=='f') {
					iero = 0
					#write (*,*) "DEBUG: tetracorder tf found"
					if(iopcon(j-1:j+2) == "tf1=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf1="
						j=j+3
						itmpo=1
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf2=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf2="
						j=j+3
						itmpo=2
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf3=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf3="
						j=j+3
						itmpo=3
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf4=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf4="
						j=j+3
						itmpo=4
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf5=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf5="
						j=j+3
						itmpo=5
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf6=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf6="
						j=j+3
						itmpo=6
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf7=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf7="
						j=j+3
						itmpo=7
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf8=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf8="
						j=j+3
						itmpo=8
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}
					if(iopcon(j-1:j+2) == "tf9=" ) {
						#write (*,*) "DEBUG: tetracorder feature tf9="
						j=j+3
						itmpo=9
						call gettetf(j,itmpo,iero)
						#call crtin   # DEBUG
						go to 9999   # go to beginning
					}

					go to 9999   # go to beginning as a bd= can be the only thing on the line
				}
			return
# ZZZZZ end defines

9999    igo = 9999
	return
	end
