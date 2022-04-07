	subroutine chinst(i,x,ic)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine inserts channels for f14.
#ccc  algorithm description:  none
#ccc  system requirements:    none
#ccc  subroutines called:
#ccc                    wjfren,crtin
#ccc  argument list description:
#ccc     arguments: i,x,ic
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#     *************************************
#     *
#     * routine for f14 to insert channel
#     *
#     *************************************

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl3"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	real*4 dataf(SPMAXCHAN)
	equivalence (dataf,datsc1)

	logical more


	call wjfren(i,x,ic)
	if (ic==ihe || ic==ihx) return

#     *** check for illegal entry ***
      if (ic!=0 || x<0.0 || x>=maxchn) {
		write(ttyout,10)
		return
      }

#     *** insert till user just hit carriage return ***
	in = x
	j = 0
	more = .true.
	while  (more)  {
		call crtin
		i = 1
		call wjfren(i,x1,ic1)
		i1 = i
		call wjfren(i,x2,ic2)

#        *** look for soft or hard exit ***
		if (ic1==ihe || ic2==ihe) {
			ic = ihe
			return
		} else if (ic1==ihx || ic2==ihx) {
			ic = ihx
			return
#        *** check if no more insertions ***
		} else if (i1 >= 80) {
			more = .false.
		} else if (ic1!=0 || ic2!=0 ||
		    (ictrl==ihe && i>=80)) {
			write(ttyout,10)
		} else {
			j = j + 1
			data(j) = x1
			if (ictrl == ihe) dataf(j) = x2
			if (j >= maxchn) more = .false.
		}
	}

	if (j != 0) {
#        *** copy values before where inserted ***
		if (in != 0) {
			do  k = 1,in {
				dataa(k) = datac(k)
				if (ictrl == ihe) datab(k) = error(k)
			}
		}

#        *** copy insertions ***
		l = in + j
		in = in + 1
		j = 1
		do  k = in,l {
			dataa(k) = data(j)
			if (ictrl == ihe) datab(k) = dataf(j)
			j = j + 1
		}

#        *** copy rest of array ***
		l = l + 1
		do  k = l,maxchn {
			dataa(k) = datac(in)
			if (ictrl == ihe) datab(k) = error(in)
			in = in + 1
		}

#        *** transfer to <datac> and <error> ***
		do  k = 1,maxchn {
			datac(k) = dataa(k)
			error(k) = datab(k)
		}
	}
	return
10      format(' illegal entry. reenter last entry.'/)
	end
