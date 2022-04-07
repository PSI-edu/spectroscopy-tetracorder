    real*4 function delx (wav,band,spec,i,nchans)
    implicit integer*4 (i-n)
    real*4 wav(nchans),band(nchans),spec(nchans)
#
#
#
#     i = subscript of channel to find delta of x
#     nchans = maximum number of channels
#
    if (i<1 || i>nchans || nchans <2) {
	delx=0.0
	return
    }

    if (i==1) {
        istart = 2
        iend = nchans
100     continue
        for (j=istart; j<=iend; j=j+1) {
            if (wav(j)==-1.23e34 ||
                band(j)==-1.23e34 ||
                spec(j)==-1.23e34) next 1
            break 1
        }
	if (j > nchans) {
		delx=0.0
		return
        }
	if (wav(i)==-1.23e34 || wav(j)==-1.23e34) {
		delx=0.0
		return
        }
		delx=abs(wav(i) - wav(j))
		return
    }
    if (i==nchans) {
        istart = nchans-1
        iend = 1
        inc = -1
200     continue
        for (j=istart; j>=iend; j=j-1) {
            if (wav(j)==-1.23e34 ||
                band(j)==-1.23e34 ||
                spec(j)==-1.23e34) next 1
            break 1
        }
	if (j < 1) {
		delx=0.0
		return
        }
	if (wav(i)==-1.23e34 || wav(j)==-1.23e34) {
		delx=0.0
		return
        }
		delx=abs(wav(i) - wav(j))
		return
    }
    for (j=i+1; j<=nchans; j=j+1) {
        if (wav(j)==-1.23e34 ||
            band(j)==-1.23e34 ||
            spec(j)==-1.23e34) next 1
        break 1
    }
    if (j>nchans) {
        istart = i-1
	if (istart < 1) {
		delx=0.0
		return
        }
        iend = 1
        goto 200
    }
    for (k=i-1; k>=1; k=k-1) {
        if (wav(k)==-1.23e34 ||
            band(k)==-1.23e34 ||
            spec(k)==-1.23e34) next 1
        break 1
    }
    if (k<1) {
        istart = i+1
	if (istart > nchans) {
		delx=0.0
		return
        }
        iend = nchans
        goto 100
    }
    if (wav(j)==-1.23e34 || wav(k)==-1.23e34) {
		delx=0.0
		return
    }
    delx=abs(wav(j) - wav(k))*0.5
    return
    end
