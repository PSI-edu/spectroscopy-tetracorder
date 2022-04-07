#ccc	Version: @(#)label1.h	1.3 8/8/85 10:57:13	
        common/label1/  ititl, cta, ctb, sta, stb, datea
        common/label1/  dateb,revs,filno,ira,idec,irmas
        common/label1/  ifut,itimch,ihist,mhist,nruns
        common/label1/  ieros,iwtrns,xnrm,scatim,timint,xfut
        common/label1/  data

        character*40    ititl
        character*2     cta(3),ctb(3),sta(3),stb(3),datea(3),dateb(3)
        character*60    ihist
        character*296   mhist
        integer*2       revs,filno,ira(3),idec(3),irmas
		integer*2		ifut(7),itimch,nruns,ieros,iwtrns
		real			xnrm,scatim,timint,xfut(7),data(256)
