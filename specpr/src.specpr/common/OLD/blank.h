#     explanation of common data:
#       unlabeled common:
#       dataa and datab are the data arrays which the data is put in for
#       an operation ( such as a division, addition, etc. ) and datac is
#       the resultant data calculated.
#       ititl1 ,ititl2 and ititle are the corresponding titles.
#       revs1 and revs2 are the corresponding number of revolutions (number
#         of times the spectrum has been scaned ) of the spectrometer for
#         that data run.
#       ... similarily for iraa, irab, : the right ascension time in seconds.
#       ... and the declination : ideca, idecb ; in arc seconds.
#       ... and the air mass : irmasa, irmasb ; the air mass times 1000.
#           (irmasa and irmasb are integers).
#       ... ihista, ihistb ; the histories fo the 2 files.
#       ... mhista, mhistb ; the manual histories of the two files
#       ... ifl1 and ifl2 are the 2 file (record) numbers.
#       ... ihistc : is the history used only in starpacks.
#       a starpack consists of the 3 data arrays in unlabeled common with a
#         history consisting of ihista, ihistb, and ihistc.
#       ifilex is no longer in use, formerly a file protection variable, see
#          /lblprt/ common.
#       ifutx (32) are unused variables.  the other varialbles correspond to
#          those in /label1/ common with a and b at the end of each variable
#          corresponding to dataa and datab, respectively.
#

        common dataa(4864), datab(4864), datac(4864)
        common revs1, revs2, xnrma, xnrmb, sctma, tminta, tmintb 
        common ifilex, ifl1, ifl2
        common iraa(3),irab(3),ideca(3),idecb(3),irmasa
        common irmasb, itmcha, nruna
        common iwtrna, iwtrnb, ifutx(32)
        common /ch1/ ititl1, ititl2, ititle, ihista, ihistb, ihistc
        common /ch2/ mhista, mhistb

	real*4		dataa,datab,datac
	real*4		revs1,revs2,xnrma,xnrmb,sctma,tminta,tmintb
	integer*4	ifilex,ifl1,ifl2,iraa,irab,ideca,idecb,irmasa
	integer*4	irmasb,itmcha,nruna
	integer*4	iwtrna,iwtrnb,ifutx
        character*40    ititl1,ititl2,ititle
        character*60    ihista,ihistb,ihistc
        character*296   mhista,mhistb
