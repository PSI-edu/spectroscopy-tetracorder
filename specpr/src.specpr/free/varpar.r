    function varpar(idummy)

    implicit integer*4 (i-n)
    integer*4 varpar
########################################################################
#                                                                      #
# This routine parses a numeric variable substitution command          #
#                                                                      #
########################################################################
    include "../common/lbl4"
    include "../common/cmd"
    include "../common/lundefs"
    include "../common/lblprt"
    include "../common/subvar"
    include "../common/alphabet"

    character*80 work,work2,atemp
    character*1 icnull,ctemp
    integer*4 intout,iname
    real*4 x,c,a

    atemp = iopcon

    i = 1
    k=1
	work=' '
	work2=' '
    while(i < 80 ) {
        if (iopcon(i:i) == ';') {
            do j = i,80 {
                work(k:k) = iopcon(j:j)
                k=k+1
            }
            goto 1000
        } else if (iopcon(i:i) == '{') {
            j=index(iopcon,'}')
            if (j==0) {
                varpar = -1
                goto 2000
            }
            c = 0
            ii = i+1
            call wjfren(ii,x,il)
            intv = 1
            intvar = 1
            iset =0
            while(ii < j) {
                if (iopcon(ii-1:ii-1) == '=') {
                    c = 0
                    iname = in
                    intvar = intv
                    iset =1
                } else if (iopcon(ii-1:ii-1) == '+') {
                    c = c +x
                } else {
                    if (iopcon(ii-1:ii+3) == 'iprtd') {
                        c = c + iprtd
                        ii=ii+4
                    } else if (iopcon(ii-1:ii+3) =='iprtu') {
                        c = c + iprtu
                        ii=ii+4
                    } else if (iopcon(ii-1:ii+3) =='iprtv') {
                        c = c + iprtv
                        ii=ii+4
                    } else if (iopcon(ii-1:ii+3) =='iprtw') {
                        c = c + iprtw
                        ii=ii+4
                    } else if (iopcon(ii-1:ii+3) =='iprty') {
                        c = c + iprty
                        ii=ii+4
                    } else if (iopcon(ii-1:ii+3) =='iprts') {
                        c = c + iprts
                        ii=ii+4
                    } else if (iopcon(ii-1:ii-1) == 'i' &&
                        iopcon(ii:ii) >= 'a' && iopcon(ii:ii) <= 'z') {
                            in =ichar(iopcon(ii:ii))-ichar('a')+1
                            c=c+ivars(in)
                            ii=ii+1
                    } else if (iopcon(ii-1:ii-1) == 'r' &&
                        iopcon(ii:ii) >= 'a' && iopcon(ii:ii) <= 'z') {
                            in = ichar(iopcon(ii:ii))-ichar('a')+1
                            intv = 0
                            c=c+rvars(in)
                            ii=ii+1

                    } else {
                        varpar = -1 
                        goto 2000
                    }
                }
                call wjfren(ii,x,il)
            }
            c=c+x
            if (iset == 0) {
                if(intvar == 1) {
                    write(work2,*) int(c)
                } else {
                    write(work2,*) c
                }
                do ii = 1,80 {
                    ctemp=work2(ii:ii)
                    work2(ii:ii) = ' '
                    if (ctemp != ' ') {
                        work(k:k) = ctemp
                        k = k +1
                    }
                }
            } else {
                if (intvar ==1 ) {
                    ivars(iname) = int(c)
                } else {
                    rvars(iname) = c
                }
                varpar = 1
                return
            }
            i = j
        } else {
		work(k:k) = iopcon(i:i)
		k=k+1
	}
		i=i+1
    }
1000    iopcon = work
	write(6,*) iopcon
	varpar = 0
	return
2000    iopcon = atemp
    varpar = -1
    return
    end
