	subroutine runsb1
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lblg"


	do j = 1,nchans {
		datac(j) = dataa(j)-datab(j)
		if (abs(datac(j))<0.1e-35) datac(j) = 0.0
		if (dataa(j)==(-1.23e34)||datab(j)==(-1.23e34))
			datac(j) = -1.23e34
	}
	revs = revs1
	mhist = mhista
	itimch = itmcha
	nruns = nruna
	iwtrns = iwtrna
	xnrm = xnrma
	scatim = sctma
	timint = tminta
	irmas = irmasa
	do j = 1,3 {
		cta(j) = cta1(j)
		ctb(j) = ctb1(j)
		sta(j) = sta1(j)
		stb(j) = stb1(j)
		ira(j) = iraa(j)
		idec(j) = ideca(j)
		datea(j) = datea1(j)
		dateb(j) = dateb1(j)
	}
	return
	end
