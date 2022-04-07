subroutine wdgsb2(iwdflg)
implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s):  roger clark & jeff hoover
#ccc  language:   fortran
#ccc
#ccc  short description:
#ccc                   this subroutine initializes different parameters
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    crtin,wjfren
#ccc  argument list description:
#ccc      argument: iwdflg
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
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/lblprt"
	include "../common/alphabet"
	include "../common/lundefs"

logical iwdflg
iwdflg = .false.

repeat {
	call whedr
	write(ttyout,10)
#
#     free format input of ifilex
#
	call crtin
	i = 1
	repeat {
		il=0
		call wjfren(i,x,il) # expect il= v, w, d, u, y, s
		ifollow = 0
		repeat {
			call wjfren(i,x,im)
			#write (ttyout,'("debug1: i=",i6, a, f12.4)') i, il, x
			if (im != ihf) {                     # check for following
				call wjfren(i,x2,ifollow)
				if (ifollow != ihf) i = i-1  # f not found, so back up
			} else {
				ifollow = ihf
				im = 0
			}
			#write (ttyout,'("debug2: i=",i6, a, f12.4)') i, il, x
			if (il==ihx || il==ihe)      go to 910
			if (i>=80 && ifollow != ihf && il == 0) go to 910
			if (il==0 || x<-maxrec || x>maxrec) go to 900
			if (x >= -1 && ifollow == ihf) {
				write(ttyout,222)
222				format(' ERROR: can only follow read only files',/)
				go to 900
			}
			if (il!=ihv&&il!=ihw&&il!=ihd&&il!=ihu&&il!=ihy&&il!=ihs) {
				#write(ttyout,'("Note change protection: did not find v, w, d, u, y, or s")')
				go to 900
			}
			if (il==ihv) {
				iprtv = x
				#write(ttyout,'("v-protection now=" i7)') iprtv
				if (ifollow == ihf && x < -1) {
					vfollow = 1   # follow growing file
					write (ttyout, 333) il
333					format (' Will follow growing file ',a1,/)
				}
				if (iprtv > -1) vfollow = 0  # do not follow
			}
			if (il==ihw) {
				iprtw = x
				if (ifollow == ihf && x < -1) {
					wfollow = 1   # follow growing file
					write (ttyout, 333) il
				}
				if (iprtw > -1) wfollow = 0  # do not follow
			}
			if (il==ihd) {
				iprtd = x
				if (ifollow == ihf && x < -1) {
					dfollow = 1   # follow growing file
					write (ttyout, 333) il
				}
				if (iprtd > -1) dfollow = 0  # do not follow
			}
			if (il==ihu) {
				iprtu = x
				if (ifollow == ihf && x < -1) {
					ufollow = 1   # follow growing file
					write (ttyout, 333) il
				}
				if (iprtu > -1) ufollow = 0  # do not follow
			}
			if (il==ihy) {
				iprty = x
				if (ifollow == ihf && x < -1) {
					yfollow = 1   # follow growing file
					write (ttyout, 333) il
				}
				if (iprty > -1) yfollow = 0  # do not follow
			}
			if (il==ihs) {
				iprts = x
			}
			if (il==ihv) {
				ifilex = x   # ifilex is no longer in use (circa 1990 or earlier), ignore this
			}
			if (i>=80) go to 910
			if (im!=0) il = im
		} until(im==0)
	}
900 continue
}
910   continue
if (ititle(1:2)=="zx") iwdflg = .true.
else {
	bbnd = 0
	ubnd = 2
	nchans = 256
	ira(1) = 0
	ira(2) = 0
	ira(3) = 0
	idec(1) = 0
	idec(2) = 0
	idec(3) = 0
	irmas = 0
	iwch = 0
	ra = 0
	dec = 0
	ha = 0
	airmas = 0
}
return

10  format(1x,
	'FILE PROTECTION: type in the file id and',
	' protection (e.g. v237 w-1 y-23f)',/,
	' protection number:',//,
	' PROTECTED, WRITE AT END OF DATA: protection',
	' number 0 or greater',/,
	' READ ONLY: protection number less than -1',
	' (you canot have',/,
	'            a read only file of 1 record)',/,
	' UNPROTECTED: protection number = -1',/,
        ' include  f  after read-only protection to follow a growing file',/)


end
