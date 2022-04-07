
         
        integer*4 wflag,eflag,cflag,dflag,head,should,aflag,band,oflag
        integer*4 ferror,ier,verb,ttyin,ttyout
# RED renamed string to strbuf
        character*80 strbuf,filnam,ofile
        real*4 widmin,widmax,errmin,errmax,asymin,asymax
	real*4 depmin,depmax,cenmin,cenmax
      
	common /spfeat1/ wflag,eflag,cflag,dflag,head,band,oflag
	common /spfeat1/ ferror,aflag,verb
	common /spfeat1/ widmin,widmax,errmin, errmax ,asymin,asymax
	common /spfeat1/ cenmin,cenmax, depmin,depmax
        common /spfeat1/ filnam,ofile,ttyin,ttyout
