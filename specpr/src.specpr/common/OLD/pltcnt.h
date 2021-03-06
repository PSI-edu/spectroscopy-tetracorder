###### gould/hp plotting related variables ######
# Version date: %W% %G% %U%

common /pltcnt/ wminp,wmaxp,vminp,vmaxp,vscale,hscale   #real
common /pltcnt/	down									#real
common /pltcnt/ ilog,penplt,nhscle                      #logical
common /pltcnt/ pltcnt,ldel(4864),gdel(4864),iptype,nplots #integer
common /pltcnt/ xlabel,vlabel                           #character

real	wminp,wmaxp,vminp,vmaxp,vscale,hscale,down
logical ilog,nhscle
integer*4 pltcnt,ldel,gdel,iptype,nplots,penplt
character xlabel*60,vlabel*60

common /pltdta/ textx(15),texty(15)                     #real
common /pltdta/ wvmx,wvmn								#real
common /pltdta/ nolim,newplt,delete,txtflg       #logical
common /pltdta/ recno,wavid,wavrec,linetp,ptsize,conct,errbar        #integer
common /pltdta/ symtyp,texta(15),texts(15)    			#integer
common /pltdta/ more,texte(15),filid,chans              #integer
common /pltdta/ pltopt,text 							#character

real textx,texty
real wvmx,wvmn
logical nolim,newplt,delete,txtflg
integer*4 recno,wavid,wavrec,linetp,ptsize,conct,errbar
integer*4 symtyp,texta,texts 
integer*4 more,texte,filid,chans
character text*300,pltopt*20

character opt*548
equivalence (opt,textx)
