    subroutine f15(ic,iprodp)
    implicit integer*4 (i-n)
#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This is special function f15.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc      rstart,hreset,crtin,wjfren,setspool,pdata,f15cde
#ccc  argument list description:
#ccc     arguments: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#     gaussian fit formatter
#

	include "../common/spmaxes"   # max parameters, must be first

    include "../common/blank"
    include "../common/lbl6"
    include "../common/lbl3"
    include "../common/label1"
    include "../common/lundefs"
    include "../common/alphabet"


	character*6 alpcde
    integer*2 sub,sb
    integer*2 coneps,conpos,gsepos,guspos

    logical invwvl,logbse,flag

    data invwvl, logbse, flag /.false.,.false.,.false./


10  call hreset(1)
    write(ttyout,1)
    write(ttyout,15) ititl,idv1,ifl1

    if (ictrl==-1) {
        write(ttyout,35)
        call crtin
        goto 1000
    }

30  if (ihist(1:4)!=' gfi') {
#
#     *** error has occured on input file ***
#
        write(ttyout,45) ihist(1:4)
        call crtin
        go to 1000
    }

50  if (iprodp!=1) {
        write(ttyout,55)
        call crtin
        i=1
        call wjfren (i,x,il)
        if (il==ihe) go to 1000
        if (il==ihx) go to 1001
    }
#
#     ********************* variable definitions ********************
#     *  ngaus=number of gaussian terms                             *
#     *  ncont=   "   "  continuum  "                               *
#     *  nerpos=position of beginning of errors for height, center, *
#     *     and width of gaussian terms in list.                    *
#     *  alpcde is alpha var. used to print out the number of the   *
#     *     gaussian term.                                          *
#     *  invwvl=switch indicating if fit was done in inv. wavelength*
#     *  logbse=switch indicating if fit was done in log base e sp. *
#     *  data1-7 are corrected values based on above conditions     *
#     *  flag is check for zero denominator in equations.           *
#     ***************************************************************
#
#     *** initialize routine  ***
#
56  n=1
    ngaus=dataa(n)				# number of gaussians
    ncont=dataa(n+1)			# number of cont. terms
	guspos=3					# start position of gauss. terms
    conpos=ngaus*3+guspos		# start position of cont. terms
	gsepos=conpos+ncont			# start position of gauss. err. terms
    coneps=ngaus*3+gsepos		# start position of cont. err. terms
    intpos=coneps+ncont			# start position of int. inten.
    mnpos=2*ngaus+intpos		# start position of mean,sigma

#     ***  begin printout  ***
#
    lpline=0
    call setspo
    call pdata (0,1,0,0,filno,lpline)

#     *** check to see if there are any gaussians to print ***
#
    if (ngaus==0) {
        write (lstlun,114)
114     format (57x,'no gaussian terms')
        go to 500
    }

    write (lstlun,105)
105 format (52x,'g a u s s i a n   t e r m s')
    write (lstlun,111) mhist(223:248),mhist(251:270)
111 format (42x,a,a)

110 write (lstlun,115)
115 format (21x,'height',15x,'error',15x,'center',15x,'error',16x,
                'width',15x,'error')

    do k=1,3*ngaus,3 {
		sub = k-1
        call f15cde(alpcde,k)
        write (lstlun,145) k,alpcde,
			dataa(sub+guspos),dataa(sub+gsepos),
            dataa(sub+guspos+1),dataa(sub+gsepos+1),
			dataa(sub+guspos+2),dataa(sub+gsepos+2)
145     format (1x,i2,a,5x,' h= ',1pg12.5,3x,' +/- ',1pg12.5,5x,
        ' c= ',1pg12.5,3x,' +/- ',1pg12.5,5x,' w= ',1pg12.5,3x,' +/- ',
        1pg12.5)
    }
#
#     *** print continuum terms ***
#     *** check to see if there are any continuum terms to print ***
#
500 if (ncont==0) {
        write (lstlun,503)
503     format (57x,'no continuum terms')
        go to 160
    }
#
#
#
    write (lstlun,505)
505 format (40x,'c o n t i n u u m  t e r m s  a n d  e r r o r s')
    write (lstlun,507)
507 format (15x,'constant')
    if (ncont>=1) write (lstlun,510) (j,j=1,ncont-1)
510 format (32x,i1,'st order',10x,i1,'nd order',10x,i1,
                       'rd order',10x,i1,'th order')
#
#     *** print continuum terms ***
#
    write (lstlun,525) (dataa(j+conpos),j=0,ncont-1)
525 format (1x,'cont. :',5x,1pg12.5,4(7x,1pg12.5))
#
#     *** print continuum errors ***
    write (lstlun,555) (dataa(j+coneps),j=0,ncont-1)
555 format (1x,'errors:',5x,1pg12.5,4(7x,1pg12.5))
#
#
#     *** print intensities of gaussians **
#     *** check to see if there are gaussian intensities to print ***
160	if (ngaus==0) go to 205
    write (lstlun,165)
165 format(10x,'intergated intensities of the gaussians (=h*w)',
                     11x,'% error')
    do k=1,2*ngaus,2 {
        sub=k-1
        call f15cde(alpcde,k)
#     check for zero denominator in equations ***
        perror=dataa(sub+intpos)
        if (perror==0) {
            perror=0.1e-36
            flag=.true.
        }
        perror=abs(dataa(sub+intpos+1)/perror)*100
        write (lstlun,175) k,alpcde,dataa(sub+intpos),
        dataa(sub+intpos+1),perror
175     format (1x,i2,a,5x,1pg12.5,' +/- ',1pg12.5,24x,0pf8.4,'%')
    }
    if (flag) write(ttyout,225)
225 format (1x,'*** error-- denominator in equation is zero.',
               '  number is reset to 0.1e-37.')
#
#
#     *** print mean and sigma ***
#
205 write (lstlun,215) dataa(mnpos), dataa(mnpos+1)
215 format (1x,'mean= ',1pg12.5,10x,'sigma= ',1pg12.5)
#
#     **************************************************************
#     *  check for inverse wavelength space and log base e space.  *
#     *  only one set of checks is necessary depending on how data *
#     *  is set up.  data must be recalculated if it is not in the *
#     *  proper space.  new values are put in data1-9.             *
#     **************************************************************
#
#
    if (mhist(223:224)==' i') invwvl=.true.
    if (mhist(251:252)=='lo') logbse=.true.

    if (ngaus==0) go to 1000

    write (lstlun,600)
600     format (23x,'g a u s s i a n   p a r a m e t e r s  i n ',
        ' w a v e l e n g t h  l i n e a r  s p a c e')

    flag=.false.
    j=-1
    n=1
    if (invwvl) {
#     *** fit was done in inverse wavelength space ***
        write (lstlun,615)
615             format (13x,'center wvlngth',3x,'wvlngth+error',3x,
            'wvlngth-error',3x,'% error in wvlngth',7x,'width',12x,
            '% width',5x,'% error in width')

        do m=1,ngaus {
            j=j+3
            sub=n+j
            data1=dataa(sub+1)
            data2=(dataa(sub+1)+(dataa(sub+1+gsepos)))
            data3=(dataa(sub+1)-(dataa(sub+1+gsepos)))
            data4=dataa(sub+2)/data1*100
            data5=dataa(sub+2)
            perr2=dataa(sub+1)
            data6a=dataa(sub+1)-(dataa(sub+2)/2)
            data6b=dataa(sub+1)+(dataa(sub+2)/2)
#
#     *** check for zero denominators in equations ***
            if (data1==0.0) {
                data1=0.1e-37
                flag=.true.
            }
            data1=1/data1
            if (data2==0.0) {
                data2=0.1e-37
                flag=.true.
            }
            data2=1/data2
            if (data3==0.0) {
                data3=0.1e-37
                flag=.true.
            }
            data3=1/data3
            if (data6a==0.0) {
                data6a=0.1e-37
                flag=.true.
            }
            data6a=1/data6a
            if (data6b==0.0) {
                data6b=0.1e-37
                flag=.true.
            }
            data6b=1/data6b
            if (perr2==0.0) {
                perr2=0.1e-37
                flag=.true.
            }
            perr2=dataa(sub+1+gsepos)/perr2*100
            if (data5==0.0) {
                data5=0.1e-37
                flag=.true.
            }
            data5=dataa(sub+2+gsepos)/data5*100
            data6=data6a-data6b

            if (flag) write(ttyout,625)
625             format (1x,'*** error-- denominator in equation is zero.',
            '  number is reset to 0.1e-37.')

            call f15cde(alpcde,m)
            write (lstlun,635) m,alpcde,data1,data2,data3,
                perr2,data6,data4,data5
635     format (2x,i2,a,7x,3(1pg12.5,4x),0pf8.4,'%',10x,1pg12.5,8x,
     0pf8.4,'%',7x,0pf8.4,'%')
        }
    }

    if (logbse) {
#     *** fit was done in log base e space ***
        write (lstlun,705)
705             format (35x,'g a u s s i a n   d e p t h s   i n   l i n',
            ' e a r  s p a c e')
        write (lstlun,715)
715		format (18x,'relative band depth=1-exp(h)',12x,
        'band depth-error=1-exp(h-e)',13x,'band depth+error=1-exp(h+e)')
        j=-1
        n=1
        do m=1,ngaus {
            j=j+3
            sub=n+j
            data7=1-exp(dataa(sub))
            data8=1-exp(dataa(sub)-dataa(sub+gsepos))
            data9=1-exp(dataa(sub)+dataa(sub+gsepos))
            call f15cde(alpcde,m)
            write (lstlun,735) m,alpcde,data7,data8,data9
735                     format (2x,i2,a,16x,2(1pg12.5,28x),1pg12.5)
        }
    }

710	continue
#
#     *** end of routine ***
#
    write(ttyout,765)
765	format (1x,'end of subroutine f15.')


1000	continue
1001	ic=ihx
9000	call dumpsp
	return

1       format (' this is special function f15.',/,
        ' this routine formats the gaussian parameter file:',/)

15      format (1x,a,5x,'file= ',a,i4)

35      format (' *** error - no data file on input. ***',/,
        '     program will soft exit.')

45      format (' *** error has occured on input file ***',/,
        '    wrong data file.  ihist=',a,
        /,5x,'program will soft exit.')

55      format (' press return to continue, e, or x to exit')
    end
