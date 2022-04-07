	subroutine shomenu1(j, il, igo)

	implicit none

#ccc  version date: 11/26/2018
#ccc  author(s): roger clark
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc            this subroutine decodes show stuff
#ccc            redraw show showov showbd showtet showall showmenu
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc  argument list description:
#ccc     argumrnts: j, il, igo
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

include "../common/lbl4"
include "../common/lundefs"
include "../common/alphabet"
include "../common/overlys"
include "../common/sp3pfeat"
include "../common/tetfeat"
include "../common/wavemarks"
include "../common/deletep"
include "../common/spcolors"


integer*4 il, j, igo

igo =0

				if (j < 50 && il == ihr) {   # redraw
					if (iopcon(j-1:j+4) == "redraw") {
						igo = 9999
						return
					}
				}
				if (j < 50 && il == ihs) {   # show  
					if (iopcon(j-1:j+4) == "show  ") {
						ovflgmenu = 0
						wmflgmenu = 0
						bdflgmenu  = 0
						tetflgmenu = 0
						igo = 9999
						return
					}
				}
				if (j < 50 && il == ihs) {   # showov
					if (iopcon(j-1:j+4) == "showov") {
						ovflgmenu = 1
						igo = 9999
						return
					}
				}
				if (j < 50 && il == ihs) {   # showwm
					if (iopcon(j-1:j+4) == "showwm") {
						wmflgmenu = 1
						igo = 9999
						return
					}
				}
				if (j < 50 && il == ihs) {   # showbd
					if (iopcon(j-1:j+4) == "showbd") {
						bdflgmenu  = 1
						igo = 9999
						return
					}
				}
				if (j < 50 && il == ihs) {   # showtet
					if (iopcon(j-1:j+5) == "showtet") {
						tetflgmenu = 1
						igo = 9999
						return
					}
				}
				if (j < 50 && il == ihs) {   # showall
					if (iopcon(j-1:j+5) == "showall") {
						wmflgmenu = 1
						ovflgmenu = 1
						bdflgmenu  = 1
						tetflgmenu = 1
						igo = 9999
						return
					}
				}
				if (j < 50 && il == ihs) {   # showmenu
					if (iopcon(j-1:j+6) == "showmenu") {
						wmflgmenu = 0
						ovflgmenu = 0
						wmflgmenu = 0
						bdflgmenu  = 0
						tetflgmenu = 0
						igo = 9999
						return
					}
				}
	return
	end
