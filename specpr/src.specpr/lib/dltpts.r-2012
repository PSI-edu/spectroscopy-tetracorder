#     subroutine to delete channels. latest version: 05/04/79
subroutine dltpts(i,j,idlt,nchans,ic)
implicit integer*4 (i-n)
#     *****************************************************************
#     *
#     * routine deletes channels. user types a channel number, or
#     * a range of channels to be deleted by typing 't' between
#     * the two the upper and lower range (e.g. 1 t 5). if both
#     * upper and lower bounds are equal, one channel will be
#     * deleted. typing a 'c' will return to program and continue.
#     * input:
#     *   i      - position in iopcon
#     *   nchans - number of points in array (used to test to see
#     *            user doesn't try to delete an outside point)
#     *
#     * output:
#     *   j      - number of channels to be deleted. calling program
#     *            should check to see if 0 (do 10 i = 1,0 will
#     *            iterate once)
#     *   idlt   - array of channels to be deleted.
#     *   ic     - return flag...
#     *            e or x: exit
#     *            c     : continue
#     *
#     *****************************************************************
	include "../common/lbl4"
	include "../common/alphabet"
	include "../common/lundefs"

integer*4 idlt(nchans)

itflg = 0
j = 0
repeat {
	if (i<80) {
		call wjfren(i,x,ic)
		if (ic==ihe||ic==ihx||ic==ihc&&x==0.0)
			return
		if (ic!=0&&ic!=iht||x<0.0||x>nchans) {
			if (ic==ihc&&x>0.0&&x<=nchans) {
				ic = 0
				i = i-1
			}
			else {
				call what(i)
				write(ttyout,20)
				itflg = 0
				j = 0
				go to 10
			}
		}
#     *** store number and set a flag if find a t ***
		if (itflg!=1) {
			if (ic==iht) {
				itflg = 1
				if (j == 0 && x < 1.0) {    # this is a t
                                                            # before a channel
					call what(i)
					write(ttyout,21)
					itflg = 0
					j = 0
					go to 10
				}
			}
			if (x < 1.0)
				next 1
			j = j+1
			idlt(j) = x
			if (j<nchans)
				next 1
			break 1
		}
		else
#     *** get upper limit and store range of deleted points ***
		 if (j!=0 && x==idlt(j)) {
			itflg = 0
			next 1
			}
		else if (j==0||x==0.0||x<=idlt(j)) {
			call what(i)
			write(ttyout,40)
			j = 0
			itflg = 0
			}
		else {
			ilow = idlt(j)+1
			ihigh = x
			itflg = 0
			repeat {
				j = j+1
				idlt(j) = ilow
				if (j>=nchans)
					return
				if (ilow>=ihigh)
					go to 900
				ilow = ilow+1
			}
		}
	}
#     *** get another line ***
	10  write(ttyout,30)
	call crtin
	i = 1
900   continue
}
j = nchans
return

20  format(' ILLEGAL CHARACTER OR CHANNEL NUMBER.',/,
		' REENTER ALL points to be deleted.',/)
21  format(' ILLEGAL CHARACTER: a  t  cannot be first, ',
		'a channel must be first',/,
		' REENTER ALL points to be deleted.',/)
30  format(/,' Enter more deletions or type c to continue.',/)
40  format(' ILLEGAL RANGE.  REENTER ALL points to be deleted.',/)
end
